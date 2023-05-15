"""Set of unit tests for the import_utils module.

Note that when executing these tests under coverage,
depending on the version of Python used
different parts of the tested module would appear as not tested.
To achieve full coverage it is necessary to run these tests
under both Python 2 and 3, and then combine coverage results.
"""

import os
import sys
import unittest

import latex_access.import_utils as imp_utils


class TestImportUtils(unittest.TestCase):

    HERE = os.path.abspath(os.path.dirname(__file__))

    @classmethod
    def setUpClass(cls):
        """Adds current directory to the PythonPath,
        so that fake package used for testing can be imported.
        Also imports the package,
        and adds it as a class variable,
        which can then be used in all tests.
        """
        sys.path.append(cls.HERE)
        import fake_pkg
        cls.pkg = fake_pkg
        # Restore sys.path to its previous value,
        # to avoid possible side effects for other tests.
        sys.path.remove(cls.HERE)

    def test_existing_module_found(self):
        """Module which exists in a package can be found by its name."""
        self.assertTrue(
            imp_utils.module_exists("sample_mod", self.pkg)
        )

    def test_non_existent_module_false_returned(self):
        """Correct value returned for a module which does not exist in a package."""
        self.assertFalse(
            imp_utils.module_exists("foobar", self.pkg)
        )

    def test_non_existent_package_name_marked_as_not_found(self):
        """When given a dotted (meaning name of the sub-package) name,
        which does not exiss, ``False`` is returned.
        """
        self.assertFalse(
            imp_utils.module_exists("foo.bar",self.pkg)
        )

    def test_module_can_be_imported_by_name(self):
        """Verify that the known module can be imported by its name."""
        self.assertEqual(
            imp_utils.import_module("mod_with_val", self.pkg).value, 42
        )
