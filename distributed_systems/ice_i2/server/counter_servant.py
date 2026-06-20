import Demo


class CounterI(Demo.Counter):

    def __init__(self, name: str, initial: int = 0):
        self._name = name
        self._value = initial
        print(f"[INIT] CounterI created: dedicated/{name} (value={initial})",
              flush=True)

    def getValue(self, current=None):
        print(f"[CALL] dedicated/{self._name}.getValue() -> {self._value}",
              flush=True)
        return self._value

    def setValue(self, val, current=None):
        print(f"[CALL] dedicated/{self._name}.setValue({val})", flush=True)
        self._value = val

    def increment(self, current=None):
        self._value += 1
        print(f"[CALL] dedicated/{self._name}.increment() -> {self._value}",
              flush=True)
        return self._value

    def reset(self, current=None):
        print(f"[CALL] dedicated/{self._name}.reset()", flush=True)
        self._value = 0

    def getName(self, current=None):
        return self._name
