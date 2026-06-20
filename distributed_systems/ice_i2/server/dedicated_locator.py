import Ice
from counter_servant import CounterI


class DedicatedServantLocator(Ice.ServantLocator):
    def __init__(self, adapter: Ice.ObjectAdapter):
        self._adapter = adapter

    def locate(self, current):
        ident = current.id
        print(f"[LOCATOR] locate() called for category='{ident.category}' "
              f"name='{ident.name}' op='{current.operation}'", flush=True)
        if ident.category != "dedicated":
            return None, None

        servant = CounterI(ident.name)
        self._adapter.add(servant, ident)
        print(f"[LAZY INIT] dedicated/{ident.name} (added to ASM)", flush=True)
        return servant, None

    def finished(self, current, servant, cookie):
        pass

    def deactivate(self, category):
        print(f"[LOCATOR] deactivate category='{category}'", flush=True)
