# Copyright (c) ZeroC, Inc.

# slice2py version 3.8.1

from __future__ import annotations
import IcePy

from Ice.UserException import UserException

from dataclasses import dataclass


@dataclass
class CounterNotFound(UserException):
    """
    Thrown when a counter cannot be located or restored
    (e.g. evictor failed to read state from disk).
    
    Notes
    -----
        The Slice compiler generated this exception dataclass from Slice exception ``::Demo::CounterNotFound``.
    """
    name: str = ""
    reason: str = ""

    _ice_id = "::Demo::CounterNotFound"

_Demo_CounterNotFound_t = IcePy.defineException(
    "::Demo::CounterNotFound",
    CounterNotFound,
    (),
    None,
    (
        ("name", (), IcePy._t_string, False, 0),
        ("reason", (), IcePy._t_string, False, 0)
    ))

setattr(CounterNotFound, '_ice_type', _Demo_CounterNotFound_t)

__all__ = ["CounterNotFound", "_Demo_CounterNotFound_t"]
