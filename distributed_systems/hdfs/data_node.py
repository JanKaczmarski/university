import asyncio
from typing import Dict, List, Optional

import ray

from name_node import HEARTBEAT_INTERVAL


class DataNodeError(Exception):
    pass


class NodeFailedError(DataNodeError):
    pass


class ChunkNotFoundError(DataNodeError):
    pass


@ray.remote
class DataNode:
    def __init__(self, node_id: str, name_node) -> None:
        self.node_id = node_id
        self.chunks: Dict[str, str] = {}  # chunk_id -> raw string data
        self.failed: bool = False
        self.name_node = name_node
        self._heartbeat_task: Optional[asyncio.Task] = None

        # Fire-and-forget registration — no ray.get to avoid blocking the async event loop
        # Use current_actor to pass the proper Ray actor handle (not the raw Python object)
        self._handle = ray.get_runtime_context().current_actor
        name_node.add_worker.remote(self._handle)
        asyncio.ensure_future(self._heartbeat_loop())

    def write_chunk(self, chunk_id: str, data: str) -> None:
        if self.failed:
            raise NodeFailedError(f"[{self.node_id}] Node is failed, cannot write chunk '{chunk_id}'")
        self.chunks[chunk_id] = data

    def read_chunk(self, chunk_id: str) -> str:
        if self.failed:
            raise NodeFailedError(f"[{self.node_id}] Node is failed, cannot read chunk '{chunk_id}'")
        if chunk_id not in self.chunks:
            raise ChunkNotFoundError(f"[{self.node_id}] Chunk '{chunk_id}' not found")
        return self.chunks[chunk_id]

    def delete_chunk(self, chunk_id: str) -> None:
        if self.failed:
            raise NodeFailedError(f"[{self.node_id}] Node is failed, cannot delete chunk '{chunk_id}'")
        self.chunks.pop(chunk_id, None)

    async def _heartbeat_loop(self) -> None:
        """Periodically notify NameNode that this node is alive."""
        while not self.failed:
            await self.name_node.heartbeat.remote(self._handle)
            await asyncio.sleep(HEARTBEAT_INTERVAL)

    def simulate_failure(self) -> None:
        """
        Mark node as failed and stop sending heartbeats.
        NameNode will detect the missing heartbeats and call remove_worker.
        """
        self.failed = True
        print(f"[{self.node_id}] Node failure simulated — heartbeat stopped")

    def recover(self, name_node=None) -> None:
        """
        Bring the node back online. Optionally accepts a new NameNode handle
        (e.g. if NameNode was restarted). Re-registers and restarts heartbeat.
        """
        if name_node is not None:
            self.name_node = name_node

        self.failed = False
        self.name_node.add_worker.remote(self._handle)
        asyncio.ensure_future(self._heartbeat_loop())
        print(f"[{self.node_id}] Node recovered — heartbeat restarted")

    def list_chunks(self) -> List[str]:
        return list(self.chunks.keys())

    def get_status(self) -> dict:
        return {
            "node_id": self.node_id,
            "failed": self.failed,
            "chunk_count": len(self.chunks),
            "chunk_ids": list(self.chunks.keys()),
        }
