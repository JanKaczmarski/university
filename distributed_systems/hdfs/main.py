import time

import ray

from data_node import DataNode
from name_node import NameNode

NUM_DATA_NODES = 4
NAME_NODE_HANDLE = "name_node"
NAMESPACE = "hdfs"


def main() -> None:
    ray.init(namespace=NAMESPACE)

    name_node = NameNode.options(name=NAME_NODE_HANDLE, lifetime="detached").remote()
    data_nodes = [
        DataNode.options(name=f"dn-{i}", lifetime="detached").remote(f"dn-{i}", name_node)
        for i in range(NUM_DATA_NODES)
    ]

    print(f"Waiting for {NUM_DATA_NODES} DataNodes to register...", end="", flush=True)
    while len(ray.get(name_node.list_workers.remote())) < NUM_DATA_NODES:
        time.sleep(0.1)
        print(".", end="", flush=True)
    print(" ready\n")

    print(f"NameNode   : '{NAME_NODE_HANDLE}'")
    for i in range(NUM_DATA_NODES):
        print(f"DataNode {i}  : 'dn-{i}'")

    print("\nCluster is up. Press Ctrl+C to shut down.")
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        print("\nShutting down...")
        ray.shutdown()


if __name__ == "__main__":
    main()
