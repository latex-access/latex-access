import os
import unittest
from latex_access.path import get_path

class TestPath(unittest.TestCase):
    def test_is_directory(self):
        """Test that  path is an existing directory."""
        self.assertTrue (os.path.isdir(get_path()))

    def test_is_absolute_path(self):
        "Test that  path is an absolute pathname."""
        self.assertTrue(os.path.isabs(get_path()))