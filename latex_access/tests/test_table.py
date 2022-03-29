import unittest
try:
    import latex_access.table as table_module
except SyntaxError:
    FAILED_TO_IMPORT_TABLE_MODULE = True
else:
    FAILED_TO_IMPORT_TABLE_MODULE = False


@unittest.skipIf(
    FAILED_TO_IMPORT_TABLE_MODULE,
    "Table module is incompatible with the current version of Python"
)
class TestTable(unittest.TestCase):

    def test_Build_header_row(self):
        """Test for the `BuildHeaderString` method.
        """
        self.assertEqual(
            table_module.BuildHeaderString(
                "Name & Surname & Age \\\\ \\hline \n"
            ),
            ["Name ", " Surname ", " Age   "]
        )
