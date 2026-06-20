# Copyright (c) ZeroC, Inc.

# slice2py version 3.8.1

from __future__ import annotations
import IcePy

from Demo.Counter_forward import _Demo_CounterPrx_t

from Ice.Object import Object

from Ice.ObjectPrx import ObjectPrx
from Ice.ObjectPrx import checkedCast
from Ice.ObjectPrx import checkedCastAsync
from Ice.ObjectPrx import uncheckedCast

from Ice.OperationMode import OperationMode

from abc import ABC
from abc import abstractmethod

from typing import TYPE_CHECKING
from typing import overload

if TYPE_CHECKING:
    from Ice.Current import Current
    from collections.abc import Awaitable
    from collections.abc import Sequence


class CounterPrx(ObjectPrx):
    """
    A simple stateful counter object.
    Each Ice object (identified by Ice::Identity) keeps its own value.
    
    The category part of the identity selects servant strategy:
    - "dedicated/<name>"  -> dedicated servant per object (ServantLocator + ASM)
    - "shared/<name>"     -> single default servant for all objects
    
    Notes
    -----
        The Slice compiler generated this proxy class from Slice interface ``::Demo::Counter``.
    """

    def getValue(self, context: dict[str, str] | None = None) -> int:
        """
        Returns the current counter value.
        
        Parameters
        ----------
        context : dict[str, str]
            The request context for the invocation.
        
        Returns
        -------
        int
        """
        return Counter._op_getValue.invoke(self, ((), context))

    def getValueAsync(self, context: dict[str, str] | None = None) -> Awaitable[int]:
        """
        Returns the current counter value.
        
        Parameters
        ----------
        context : dict[str, str]
            The request context for the invocation.
        
        Returns
        -------
        Awaitable[int]
        """
        return Counter._op_getValue.invokeAsync(self, ((), context))

    def setValue(self, val: int, context: dict[str, str] | None = None) -> None:
        """
        Sets the counter to the given value.
        
        Parameters
        ----------
        val : int
        context : dict[str, str]
            The request context for the invocation.
        """
        return Counter._op_setValue.invoke(self, ((val, ), context))

    def setValueAsync(self, val: int, context: dict[str, str] | None = None) -> Awaitable[None]:
        """
        Sets the counter to the given value.
        
        Parameters
        ----------
        val : int
        context : dict[str, str]
            The request context for the invocation.
        
        Returns
        -------
        Awaitable[None]
            An awaitable that is completed when the invocation completes.
        """
        return Counter._op_setValue.invokeAsync(self, ((val, ), context))

    def increment(self, context: dict[str, str] | None = None) -> int:
        """
        Increments the counter by 1 and returns the new value.
        
        Parameters
        ----------
        context : dict[str, str]
            The request context for the invocation.
        
        Returns
        -------
        int
        """
        return Counter._op_increment.invoke(self, ((), context))

    def incrementAsync(self, context: dict[str, str] | None = None) -> Awaitable[int]:
        """
        Increments the counter by 1 and returns the new value.
        
        Parameters
        ----------
        context : dict[str, str]
            The request context for the invocation.
        
        Returns
        -------
        Awaitable[int]
        """
        return Counter._op_increment.invokeAsync(self, ((), context))

    def reset(self, context: dict[str, str] | None = None) -> None:
        """
        Resets the counter to 0.
        
        Parameters
        ----------
        context : dict[str, str]
            The request context for the invocation.
        """
        return Counter._op_reset.invoke(self, ((), context))

    def resetAsync(self, context: dict[str, str] | None = None) -> Awaitable[None]:
        """
        Resets the counter to 0.
        
        Parameters
        ----------
        context : dict[str, str]
            The request context for the invocation.
        
        Returns
        -------
        Awaitable[None]
            An awaitable that is completed when the invocation completes.
        """
        return Counter._op_reset.invokeAsync(self, ((), context))

    def getName(self, context: dict[str, str] | None = None) -> str:
        """
        Returns the identity name of the counter (for demo / logging).
        
        Parameters
        ----------
        context : dict[str, str]
            The request context for the invocation.
        
        Returns
        -------
        str
        """
        return Counter._op_getName.invoke(self, ((), context))

    def getNameAsync(self, context: dict[str, str] | None = None) -> Awaitable[str]:
        """
        Returns the identity name of the counter (for demo / logging).
        
        Parameters
        ----------
        context : dict[str, str]
            The request context for the invocation.
        
        Returns
        -------
        Awaitable[str]
        """
        return Counter._op_getName.invokeAsync(self, ((), context))

    @staticmethod
    def checkedCast(
        proxy: ObjectPrx | None,
        facet: str | None = None,
        context: dict[str, str] | None = None
    ) -> CounterPrx | None:
        return checkedCast(CounterPrx, proxy, facet, context)

    @staticmethod
    def checkedCastAsync(
        proxy: ObjectPrx | None,
        facet: str | None = None,
        context: dict[str, str] | None = None
    ) -> Awaitable[CounterPrx | None ]:
        return checkedCastAsync(CounterPrx, proxy, facet, context)

    @overload
    @staticmethod
    def uncheckedCast(proxy: ObjectPrx, facet: str | None = None) -> CounterPrx:
        ...

    @overload
    @staticmethod
    def uncheckedCast(proxy: None, facet: str | None = None) -> None:
        ...

    @staticmethod
    def uncheckedCast(proxy: ObjectPrx | None, facet: str | None = None) -> CounterPrx | None:
        return uncheckedCast(CounterPrx, proxy, facet)

    @staticmethod
    def ice_staticId() -> str:
        return "::Demo::Counter"

IcePy.defineProxy("::Demo::Counter", CounterPrx)

class Counter(Object, ABC):
    """
    A simple stateful counter object.
    Each Ice object (identified by Ice::Identity) keeps its own value.
    
    The category part of the identity selects servant strategy:
    - "dedicated/<name>"  -> dedicated servant per object (ServantLocator + ASM)
    - "shared/<name>"     -> single default servant for all objects
    
    Notes
    -----
        The Slice compiler generated this skeleton class from Slice interface ``::Demo::Counter``.
    """

    _ice_ids: Sequence[str] = ("::Demo::Counter", "::Ice::Object", )
    _op_getValue: IcePy.Operation
    _op_setValue: IcePy.Operation
    _op_increment: IcePy.Operation
    _op_reset: IcePy.Operation
    _op_getName: IcePy.Operation

    @staticmethod
    def ice_staticId() -> str:
        return "::Demo::Counter"

    @abstractmethod
    def getValue(self, current: Current) -> int | Awaitable[int]:
        """
        Returns the current counter value.
        
        Parameters
        ----------
        current : Ice.Current
            The Current object for the dispatch.
        
        Returns
        -------
        int | Awaitable[int]
        """
        pass

    @abstractmethod
    def setValue(self, val: int, current: Current) -> None | Awaitable[None]:
        """
        Sets the counter to the given value.
        
        Parameters
        ----------
        val : int
        current : Ice.Current
            The Current object for the dispatch.
        
        Returns
        -------
        None | Awaitable[None]
            None or an awaitable that completes when the dispatch completes.
        """
        pass

    @abstractmethod
    def increment(self, current: Current) -> int | Awaitable[int]:
        """
        Increments the counter by 1 and returns the new value.
        
        Parameters
        ----------
        current : Ice.Current
            The Current object for the dispatch.
        
        Returns
        -------
        int | Awaitable[int]
        """
        pass

    @abstractmethod
    def reset(self, current: Current) -> None | Awaitable[None]:
        """
        Resets the counter to 0.
        
        Parameters
        ----------
        current : Ice.Current
            The Current object for the dispatch.
        
        Returns
        -------
        None | Awaitable[None]
            None or an awaitable that completes when the dispatch completes.
        """
        pass

    @abstractmethod
    def getName(self, current: Current) -> str | Awaitable[str]:
        """
        Returns the identity name of the counter (for demo / logging).
        
        Parameters
        ----------
        current : Ice.Current
            The Current object for the dispatch.
        
        Returns
        -------
        str | Awaitable[str]
        """
        pass

Counter._op_getValue = IcePy.Operation(
    "getValue",
    "getValue",
    OperationMode.Normal,
    None,
    (),
    (),
    (),
    ((), IcePy._t_int, False, 0),
    ())

Counter._op_setValue = IcePy.Operation(
    "setValue",
    "setValue",
    OperationMode.Normal,
    None,
    (),
    (((), IcePy._t_int, False, 0),),
    (),
    None,
    ())

Counter._op_increment = IcePy.Operation(
    "increment",
    "increment",
    OperationMode.Normal,
    None,
    (),
    (),
    (),
    ((), IcePy._t_int, False, 0),
    ())

Counter._op_reset = IcePy.Operation(
    "reset",
    "reset",
    OperationMode.Normal,
    None,
    (),
    (),
    (),
    None,
    ())

Counter._op_getName = IcePy.Operation(
    "getName",
    "getName",
    OperationMode.Normal,
    None,
    (),
    (),
    (),
    ((), IcePy._t_string, False, 0),
    ())

__all__ = ["Counter", "CounterPrx", "_Demo_CounterPrx_t"]
