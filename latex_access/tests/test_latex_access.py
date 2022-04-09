import unittest

import latex_access.latex_access as la_main_module


class TestTranslatorConstructor(unittest.TestCase):

    def test_table_contains_initial_values(self):
        """Test that expected keys and values are initially in the table.
        """
        tr = la_main_module.translator()
        self.assertIn("$", tr.table.keys())
        self.assertIn("\\vspace", tr.table.keys())
        self.assertIn(tr.displaystyle, tr.table.values())
        self.assertIn(tr.text, tr.table.values())

    def test_translator_initial_values(self):
        """Test that translatores constructor sets its members
        to an expected values.
        """
        tr = la_main_module.translator()
        self.assertEqual(tr.depth, 0)
        self.assertFalse(tr.remove_dollars)
        self.assertEqual(tr.space, "")
        self.assertEqual(tr.files, [])


class TestDollarsTranslation(unittest.TestCase):

    def test_dollars_preserved_by_default(self):
        """Test that by default dollars are not removed when translating.
        """
        self.assertEqual(
            la_main_module.translator().dollar("\\alpha$", 1),
            ("$", 1)
        )

    def test_dollars_removed(self):
        """Test that dollars are removed when requested.
        """
        trDollarsDiscarded = la_main_module.translator()
        trDollarsDiscarded.remove_dollars = True
        self.assertEqual(trDollarsDiscarded.dollar("\\alpha$", 1), ("", 1))

    def test_dollars_preserved(self):
        """Test that dollars are preserved when requested.
        """
        trDollarsKept = la_main_module.translator()
        trDollarsKept.remove_dollars = False
        self.assertEqual(trDollarsKept.dollar("\\alpha$", 1), ("$", 1))
