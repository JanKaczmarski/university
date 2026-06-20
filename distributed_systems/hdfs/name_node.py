import asyncio
import random
from dataclasses import dataclass, field
from typing import Dict, List, Optional

import ray

# TODO: Add functionality when chaning a single chunk we don't udpate the whole
# artefact but only the chunks affected

BLOCK_SIZE = 64
REPLICATION_FACTOR = 3
HEARTBEAT_INTERVAL = 5  # seconds
HEARTBEAT_TIMEOUT = 15  # seconds — node considered dead after this


class NameNodeError(Exception):
    pass


class ArtefactAlreadyExistsError(NameNodeError):
    pass


class ArtefactNotFoundError(NameNodeError):
    pass


class NoDataNodesAvailableError(NameNodeError):
    pass


@dataclass
class ChunkInfo:
    chunk_id: str  # e.g. "my_artefact/chunk_0"
    index: int  # chunk index within artefact
    nodes: List  # list of DataNode actor handles


@dataclass
class ArtefactMeta:
    name: str
    value_length: int
    chunks: List[ChunkInfo] = field(default_factory=list)


@ray.remote
class NameNode:
    def __init__(self) -> None:
        self.artefacts: Dict[str, ArtefactMeta] = {}
        # worker handle -> last heartbeat timestamp
        self.workers: Dict[object, float] = {}

    def add_worker(self, worker_node) -> None:
        if worker_node in self.workers:
            return
        self.workers[worker_node] = asyncio.get_event_loop().time()

    def remove_worker(self, worker_node) -> None:
        if worker_node not in self.workers:
            return

        del self.workers[worker_node]

        # Remove failed node from every chunk's replica list
        for meta in self.artefacts.values():
            for chunk in meta.chunks:
                if worker_node in chunk.nodes:
                    chunk.nodes.remove(worker_node)

        # TODO: trigger re-replication for under-replicated chunks
        # (requires client or another DataNode to re-upload — tracked separately)

    def heartbeat(self, worker_node) -> None:
        if worker_node in self.workers:
            self.workers[worker_node] = asyncio.get_event_loop().time()

    def check_heartbeats(self) -> List:
        now = asyncio.get_event_loop().time()
        dead = [node for node, last_seen in self.workers.items() if now - last_seen > HEARTBEAT_TIMEOUT]
        for node in dead:
            self.remove_worker(node)
        return dead

    def store_artefact(self, name: str, value_length: int) -> List[ChunkInfo]:
        if len(name) < 1:
            raise NameNodeError("Artefact name must be non-empty")
        if name in self.artefacts:
            raise ArtefactAlreadyExistsError(f"Artefact '{name}' already exists")

        active_workers = list(self.workers.keys())
        replication = min(REPLICATION_FACTOR, len(active_workers))
        if replication < 1:
            raise NoDataNodesAvailableError("No DataNodes available")
        if replication < REPLICATION_FACTOR:
            print(
                f"[NameNode] WARNING: only {replication} DataNode(s) available, "
                f"replication factor reduced from {REPLICATION_FACTOR} to {replication}"
            )

        num_blocks = (value_length + BLOCK_SIZE - 1) // BLOCK_SIZE

        chunks = [
            ChunkInfo(
                chunk_id=f"{name}/chunk_{i}",
                index=i,
                nodes=random.sample(active_workers, replication),
            )
            for i in range(num_blocks)
        ]

        self.artefacts[name] = ArtefactMeta(
            name=name,
            value_length=value_length,
            chunks=chunks,
        )

        return chunks

    def get_artefact(self, name: str) -> Optional[ArtefactMeta]:
        return self.artefacts.get(name)

    def get_chunk(self, name: str, chunk_index: int) -> Optional[ChunkInfo]:
        meta = self.artefacts.get(name)
        if meta is None or chunk_index >= len(meta.chunks):
            return None
        return meta.chunks[chunk_index]

    def list_artefacts(self) -> List[str]:
        return list(self.artefacts.keys())

    def list_artefact_chunks(self, name: str) -> List[dict]:
        meta = self.artefacts.get(name)
        if meta is None:
            return []
        return [
            {
                "chunk_id": c.chunk_id,
                "index": c.index,
                "replica_count": len(c.nodes),
            }
            for c in meta.chunks
        ]

    def list_workers(self) -> List[dict]:
        now = asyncio.get_event_loop().time()
        return [
            {
                "node": str(node),
                "last_heartbeat_ago_s": round(now - last_seen, 1),
            }
            for node, last_seen in self.workers.items()
        ]
