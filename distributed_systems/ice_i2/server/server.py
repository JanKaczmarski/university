#!/usr/bin/env python3
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "generated", "python"))
sys.path.insert(0, HERE)

import Ice
import Demo

from counter_servant import CounterI
from shared_counter_servant import SharedCounterI
from dedicated_locator import DedicatedServantLocator


def main():
    with Ice.initialize(sys.argv) as communicator:
        adapter = communicator.createObjectAdapter("CounterAdapter")

        locator = DedicatedServantLocator(adapter)
        adapter.addServantLocator(locator, "dedicated")

        shared = SharedCounterI()
        adapter.addDefaultServant(shared, "shared")

        adapter.activate()
        print("[SERVER] CounterAdapter activated on tcp -p 10000", flush=True)
        print("[SERVER] Categories: 'dedicated' (locator), 'shared' (default)",
              flush=True)
        print("[SERVER] Press Ctrl+C to stop", flush=True)

        try:
            communicator.waitForShutdown()
        except KeyboardInterrupt:
            print("\n[SERVER] interrupted - shutting down", flush=True)


if __name__ == "__main__":
    main()
