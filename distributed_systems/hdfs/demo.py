import ray

from client import HdfsClient
from main import NAME_NODE_HANDLE, NAMESPACE


def section(title: str) -> None:
    print(f"\n{'='*60}")
    print(f"  {title}")
    print(f"{'='*60}")


def main() -> None:
    ray.init(address="auto", namespace=NAMESPACE)

    name_node = ray.get_actor(NAME_NODE_HANDLE)
    client = HdfsClient(name_node)

    # ------------------------------------------------------------------
    # Upload
    # ------------------------------------------------------------------
    section("Upload")

    value = "A" * 200  # 200 chars → 4 chunks of 64 (last one shorter)
    client.upload("my-artefact", value)
    print(f"Uploaded 'my-artefact' ({len(value)} chars)")

    # ------------------------------------------------------------------
    # Chunk layout
    # ------------------------------------------------------------------
    section("Chunk layout (NameNode view)")

    for chunk in client.inspect_artefact("my-artefact"):
        print(f"  {chunk}")

    print("\nWorkers registered with NameNode:")
    for w in ray.get(name_node.list_workers.remote()):
        print(f"  {w}")

    print("\nChunks stored per DataNode:")
    for i in range(4):
        dn = ray.get_actor(f"dn-{i}")
        status = ray.get(dn.get_status.remote())
        print(f"  {status['node_id']}: {status['chunk_ids']}")

    # ------------------------------------------------------------------
    # Download
    # ------------------------------------------------------------------
    section("Download")

    downloaded = client.download("my-artefact")
    print(f"Downloaded 'my-artefact' ({len(downloaded)} chars)")
    print(f"Data intact: {downloaded == value}")

    # ------------------------------------------------------------------
    # Simulate node failure + fallback download
    # ------------------------------------------------------------------
    section("Simulate failure of dn-0")

    dn0 = ray.get_actor("dn-0")
    ray.get(dn0.simulate_failure.remote())
    print("dn-0 marked as failed")

    downloaded_after_failure = client.download("my-artefact")
    print(f"Download after failure ({len(downloaded_after_failure)} chars)")
    print(f"Data intact: {downloaded_after_failure == value}")

    # ------------------------------------------------------------------
    # Recover node
    # ------------------------------------------------------------------
    section("Recover dn-0")

    ray.get(dn0.recover.remote())
    print("dn-0 recovered")

    # ------------------------------------------------------------------
    # List artefacts
    # ------------------------------------------------------------------
    section("List artefacts")

    print(f"Artefacts: {client.list_artefacts()}")


if __name__ == "__main__":
    main()
