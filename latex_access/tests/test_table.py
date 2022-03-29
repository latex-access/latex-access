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

    def test_where_am_i(self):
        """Tests that information about current location in table is properly retrieved."""
        self.assertEqual(table_module.WhereAmI('4&5', ['1', '2', '3'], '1&2&3\\\\4&5&6'), 'focus is in column 2 at location B 2')

    def test_get_table_top_row(self):
        """Tests that table's top row is properly retrieved."""
        self.assertEqual(table_module.GetTableTopRow('1&2&3\\\\4&5&6'), '1&2&3')

    def test_get_table_current_row(self):
        """Tests that table's current row is properly retrieved"""
        self.assertEqual(table_module.GetTableCurrentRow('1&2&3\\4&5&6\\7&8&9'), '7&8&9')