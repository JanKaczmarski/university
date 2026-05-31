from typing import List

import ray
from ray.exceptions import RayTaskError
from ray.util import ActorPool

from data_node import ChunkNotFoundError, NodeFailedError
from name_node import BLOCK_SIZE, ArtefactMeta, ArtefactNotFoundError, ChunkInfo


class ClientError(Exception):
    pass


class UploadError(ClientError):
    pass


class DownloadError(ClientError):
    pass


class HdfsClient:
    def __init__(self, name_node) -> None:
        self.name_node = name_node

    def upload(self, name: str, value: str) -> None:
        chunks = ray.get(self.name_node.store_artefact.remote(name, len(value)))

        for chunk_info in chunks:
            data = value[chunk_info.index * BLOCK_SIZE : (chunk_info.index + 1) * BLOCK_SIZE]
            pool = ActorPool(chunk_info.nodes)
            chunk_id = chunk_info.chunk_id
            try:
                # map writes the same chunk data to every replica node in the pool
                list(pool.map(
                    lambda actor, d: actor.write_chunk.remote(chunk_id, d),
                    [data] * len(chunk_info.nodes),
                ))
            except Exception as e:
                raise UploadError(f"Failed to write chunk '{chunk_id}': {e}") from e

    def download(self, name: str) -> str:
        meta: ArtefactMeta | None = ray.get(self.name_node.get_artefact.remote(name))
        if meta is None:
            raise ArtefactNotFoundError(f"Artefact '{name}' not found")

        result_chunks: List[str] = []
        for chunk_info in sorted(meta.chunks, key=lambda c: c.index):
            data = self._read_chunk_with_fallback(chunk_info)
            result_chunks.append(data)

        return "".join(result_chunks)

    def _read_chunk_with_fallback(self, chunk_info: ChunkInfo) -> str:
        # Submit a read to every replica in parallel via ActorPool,
        # then return the first successful result (scatter-gather).
        pool = ActorPool(chunk_info.nodes)
        chunk_id = chunk_info.chunk_id
        for _ in chunk_info.nodes:
            pool.submit(lambda actor, cid: actor.read_chunk.remote(cid), chunk_id)

        errors: List[str] = []
        while pool.has_next():
            try:
                return pool.get_next_unordered()
            except (NodeFailedError, ChunkNotFoundError, RayTaskError) as e:
                errors.append(str(e))

        raise DownloadError(
            f"All replicas failed for chunk '{chunk_id}'. Errors: {errors}"
        )

    def list_artefacts(self) -> list:
        """Return names of all artefacts known to NameNode."""
        return ray.get(self.name_node.list_artefacts.remote())

    def inspect_artefact(self, name: str) -> list:
        """Return chunk layout for a given artefact."""
        layout = ray.get(self.name_node.list_artefact_chunks.remote(name))
        if not layout:
            raise ArtefactNotFoundError(f"Artefact '{name}' not found")
        return layout
