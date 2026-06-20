import os
import sys
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "generated", "python"))
sys.path.insert(0, HERE)

try:
    import Demo
except ImportError:
    raise unittest.SkipTest(
        "Generated Demo module not found - run ./generate.sh first."
    )

from counter_servant import CounterI


class CounterITests(unittest.TestCase):
    def test_initial_value(self):
        c = CounterI("Alice")
        self.assertEqual(c.getValue(), 0)
        self.assertEqual(c.getName(), "Alice")

    def test_increment_returns_new_value(self):
        c = CounterI("Bob")
        self.assertEqual(c.increment(), 1)
        self.assertEqual(c.increment(), 2)
        self.assertEqual(c.getValue(), 2)

    def test_set_and_reset(self):
        c = CounterI("Eve")
        c.setValue(42)
        self.assertEqual(c.getValue(), 42)
        c.reset()
        self.assertEqual(c.getValue(), 0)


if __name__ == "__main__":
    unittest.main()
