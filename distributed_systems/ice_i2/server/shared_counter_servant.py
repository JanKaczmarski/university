import threading
import Demo


class SharedCounterI(Demo.Counter):

    def __init__(self):
        self._lock = threading.Lock()
        self._value = 0
        print("[INIT] SharedCounterI created (one instance for category 'shared')",
              flush=True)

    def getValue(self, current=None):
        name = current.id.name if current else "?"
        with self._lock:
            v = self._value
        print(f"[SHARED] request for shared/{name}.getValue() -> {v}", flush=True)
        return v

    def setValue(self, val, current=None):
        name = current.id.name if current else "?"
        with self._lock:
            self._value = val
        print(f"[SHARED] request for shared/{name}.setValue({val})", flush=True)

    def increment(self, current=None):
        name = current.id.name if current else "?"
        with self._lock:
            self._value += 1
            v = self._value
        print(f"[SHARED] request for shared/{name}.increment() -> {v}", flush=True)
        return v

    def reset(self, current=None):
        name = current.id.name if current else "?"
        with self._lock:
            self._value = 0
        print(f"[SHARED] request for shared/{name}.reset()", flush=True)

    def getName(self, current=None):
        return current.id.name if current else ""
