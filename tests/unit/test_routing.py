import unittest
from latex_access.routing import convert, invert

class TestRouting(unittest.TestCase):
    def test_empty_list(self):
        """Testst that empty list is returned for empty input."""
        self.assertEqual(convert([]), [])

    def test_two_tuples(self):
        """Tests that right elements are returned for two tuples with three elements each."""
        self.assertEqual(convert([(1, 2, 3), (4, 5, 6)]), [2, 2, 2, 5])

    def test_three_tuples(self):
        """Tests that right elements are returned for three tuples with two elements each."""
        self.assertEqual(convert([(1, 2), (3, 4), (5, 6)]), [2, 2, 4, 4, 6])

    def test_inversion(self):
        """Tests that tuples are inverted properly."""
        self.assertEqual(invert([(0, 1)]), [(1, 0)])