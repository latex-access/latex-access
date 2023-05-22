import os
import unittest
import sys

try:
    import unittest.mock as mock_lib
except ImportError:  # Python 2 - use mock backport
    import mock as mock_lib

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
        """Test that translators constructor sets its members
        to an expected values.
        """
        tr = la_main_module.translator()
        self.assertEqual(tr.depth, 0)
        self.assertFalse(tr.remove_dollars)
        self.assertEqual(tr.space, "")
        self.assertEqual(tr.files, [])


class TestSimpleMethods(unittest.TestCase):

    """Verifies behaviour of various simple translator methods.

    By simple we mean these which do not rely on the `translate` method.
    """

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

    def test_remove_method(self):
        """Verify that `remove` method removes both command and its argument."""
        tr = la_main_module.translator()
        self.assertEqual(tr.remove(r"\hspace{1in}", 7), ("", 12))


class TestGet_arg(unittest.TestCase):
    """Set of tests for the `get_arg` function."""

    def test_start_index_outside_input_string(self):
        """Check the behaviour when starting position is out of range."""
        get_arg_res = la_main_module.get_arg("\\frac{1}2}", 12)
        self.assertEqual(get_arg_res, ("", 12, 12))

    def test_white_spaces_only_after_start_index(self):
        """Check the behaviour when provided string consists of spaces."""
        get_arg_res = la_main_module.get_arg("\\frac{1}2}    ", 12)
        self.assertEqual(get_arg_res, ("", 12, 12))

    def test_white_space_before_argument_skipped(self):
        """Ensure that white space at the start of the argument is skipped."""
        get_arg_res = la_main_module.get_arg("\\frac   {1}2}", 5)
        self.assertEqual(get_arg_res, ("1", 11, 9))

    def test_unbraced_command(self):
        """Ensure that unbraced fraction is handled."""
        get_arg_res = la_main_module.get_arg("\\frac{1}2}", 0)
        self.assertEqual(get_arg_res, (r"\frac", 5, 0))

    def test_new_line_handled(self):
        r"""Ensure that line break in a form of \\ is handled."""
        get_arg_res = la_main_module.get_arg(r"\\", 0)
        self.assertEqual(get_arg_res, ("\\", 1, 0))

    def test_starting_char_not_an_opening_brace(self):
        """Verify the behaviour when starting position is not an opening brace."""
        get_arg_res = la_main_module.get_arg("\\frac{1}2}", 7)
        self.assertEqual(get_arg_res, ("}", 8, 7))

    def test_argument_extracted_from_fraction(self):
        """Ensure that numerator is retrieved from correctly written fraction."""
        get_arg_res = la_main_module.get_arg("\\frac{1}2}", 5)
        self.assertEqual(get_arg_res, ("1", 8, 6))

    def test_nested_braces(self):
        """Verify extraction of arguments which have arguments themselves."""
        get_arg_res = la_main_module.get_arg("\\mathbb{\\frac{1}{2}}", 7)
        self.assertEqual(get_arg_res, ("\\frac{1}{2}", 20, 8))

    def test_closing_brace_missing(self):
        """Verify argument extraction when braces have not yet been closed."""
        get_arg_res = la_main_module.get_arg("\\sqrt{4", 5)
        self.assertEqual(get_arg_res, ("4", 7, 6))


class TestGetOptionalAr(unittest.TestCase):
    """Set of tests for the `get_optional_arg` function."""

    def test_starting_position_outside_range(self):
        """Verify the behaviour when the provided string is shorter than the starting pos."""
        get_opt_arg_res = la_main_module.get_optional_arg("\\sqrt[3]{8}", 12)
        self.assertEqual(get_opt_arg_res, ())

    def test_spaces_skipped(self):
        """Verify that spaces before opening bracket are skipped."""
        get_opt_arg_res = la_main_module.get_optional_arg("\\sqrt   [3]{8}", 5)
        self.assertEqual(get_opt_arg_res, ("3", 11))

    def test_command_no_optional_arg(self):
        """Ensure that when command has no optional arg empty tuple is returned."""
        get_opt_arg_res = la_main_module.get_optional_arg("\\sqrt4}", 5)
        self.assertEqual(get_opt_arg_res, ())

    def test_nested_commands_with_optional_args(self):
        """verify that optional args which have optional args themselves are extracted correctly."""
        get_opt_arg_res = la_main_module.get_optional_arg(
            "\\sqrt[\\sqrt[2]{9}]{8}",
            5
        )
        self.assertEqual(get_opt_arg_res, ("\\sqrt[2]{9}", 18))

    def test_not_closed_brackets_handled(self):
        """Verify that optional argument can be extracted when bracket has not been closed.

        Note that this examle is artificial,
        since LaTeX-access has no support for `itemize` environments.
        """
        get_opt_arg_res = la_main_module.get_optional_arg("\\item[$-$", 5)
        self.assertEqual(get_opt_arg_res, ("$-$", 10))

    def test_optional_arg_extracted_from_cube_root(self):
        """Verify that optional arg can be extracted from correctly written cube root."""
        get_opt_arg_res = la_main_module.get_optional_arg("\\sqrt[3]{8}", 5)
        self.assertEqual(get_opt_arg_res, ("3", 8))


class TestGetSubSuper(unittest.TestCase):

    """Set of tests for `get_subsuper` function."""

    def test_starting_position_outside_range(self):
        """Verify the behaviour when the provided string is shorter than the starting pos."""
        get_sub_super_res = la_main_module.get_subsuper("\\int_{a}^{b}", 13)
        self.assertEqual(get_sub_super_res, (None, None, 13))

    def test_spaces_skipped(self):
        """Verify that white space before limits of integral is skipped."""
        get_sub_super_res = la_main_module.get_subsuper("\\int   _{a}^{b}", 4)
        self.assertEqual(
            get_sub_super_res,
            (("a", 11, 9), ("b", 15, 13), 15)
        )

    def test_spaces_before_upper_limit_skipped(self):
        """Ensure that spaces after lower limit and before upper are skipped."""
        get_sub_super_res = la_main_module.get_subsuper("\\int_{a}   ^{b}", 4)
        self.assertEqual(
            get_sub_super_res,
            (("a", 8, 6), ("b", 15, 13), 15)
        )

    def test_spaces_before_lower_limit_skipped(self):
        """Ensure that spaces after upper limit and before lower are skipped."""
        get_sub_super_res = la_main_module.get_subsuper("\\int^{b}   _{a}", 4)
        self.assertEqual(
            get_sub_super_res,
            (("a", 15, 13), ("b", 8, 6), 15)
        )

    def test_integral_upper_limit_first(self):
        """Ensure that correct integral where upper limit is first is handled."""
        get_sub_super_res = la_main_module.get_subsuper("\\int^{b}_{a}", 4)
        self.assertEqual(
            get_sub_super_res,
            (("a", 12, 10), ("b", 8, 6), 12)
        )

    def test_integral_lower_limit_first(self):
        """Ensure that correct integral where lower limit is first is handled."""
        get_sub_super_res = la_main_module.get_subsuper("\\int_{a}^{b}", 4)
        self.assertEqual(
            get_sub_super_res,
            (("a", 8, 6), ("b", 12, 10), 12)
        )

    def test_integral_upper_limit_missing(self):
        """Ensure that integral with the missing upper limit is handled."""
        get_sub_super_res = la_main_module.get_subsuper("\\int_{a}", 4)
        self.assertEqual(
            get_sub_super_res,
            (("a", 8, 6), None, 8)
        )

    def test_integral_lower_limit_missing(self):
        """Ensure that integral with the missing lower limit is handled."""
        get_sub_super_res = la_main_module.get_subsuper("\\int^{b}", 4)
        self.assertEqual(
            get_sub_super_res,
            (None, ("b", 8, 6), 8)
        )

    def test_no_sub_no_super(self):
        """Verify the behaviour when the given command has neither sub nor super."""
        get_sub_super_res = la_main_module.get_subsuper("\\frac{1}{2}", 5)
        self.assertEqual(get_sub_super_res, (None, None, 5))

    def test_integral_lower_limit_missing_eol(self):
        """Ensure that integral with the missing lower limit and line break at the end is handled."""
        get_sub_super_res = la_main_module.get_subsuper(r"\int^{b}\\", 4)
        self.assertEqual(
            get_sub_super_res,
            (None, ("b", 8, 6), 8)
        )

    def test_integral_upper_limit_missing_eol(self):
        """Ensure that integral with the missing upper limit and line break at the end is handled."""
        get_sub_super_res = la_main_module.get_subsuper(r"\int_{a}\\", 4)
        self.assertEqual(
            get_sub_super_res,
            (("a", 8, 6), None, 8)
        )


TEST_TABLES_PATH = os.path.abspath(os.path.join(os.path.dirname(__file__), "test_tables"))


class TestTableFilesLocating(unittest.TestCase):

    """Set of tests for methods locating buildin table files."""

    def test_absolute_path_to_tables_dir_not_modified(self):
        """When the provided path is absolute it is returned unchanged."""
        self.assertEqual(
            "/home/user/documents/table_file.table",
            la_main_module.translator.make_table_file_path("/home/user/documents/table_file.table")
        )
        self.assertEqual(
            r"c:\users\user\documents\table_file.table",
            la_main_module.translator.make_table_file_path(r"c:\users\user\documents\table_file.table")
        )

    def test_path_to_table_made_absolute(self):
        """When only name of the table file is given, path should be made absolute based on the cwd."""
        saved_cwd = la_main_module.translator.CWD
        path = la_main_module.translator.make_table_file_path("tbl_file.table")
        self.assertTrue(os.path.isabs(path))
        self.assertTrue(path.endswith("tbl_file.table"))
        self.assertTrue(path.startswith(saved_cwd))

    def test_table_files_located_relative_to_translator_class(self):
        """Ensure that build-in ntables can be located relative to the translator class."""
        sys.path.append(TEST_TABLES_PATH)
        import testTranslator
        translator = testTranslator.TestTranslator()
        tables_dir = translator.get_buildin_tables_dir()
        self.assertEqual(
            tables_dir,
            TEST_TABLES_PATH
        )
        sys.path.remove(TEST_TABLES_PATH)
        self.assertTrue(os.path.isdir(tables_dir))
        self.assertTrue(os.path.isabs(tables_dir))


class TestTableFilesParsingAndLoading(unittest.TestCase):

    """Set of tests for loading table entries from files. """

    def setUp(self):
        """Create fresh translator instance for each of the tests."""
        sys.path.append(TEST_TABLES_PATH)
        import testTranslator
        self.translator = testTranslator.TestTranslator()
        sys.path.remove(TEST_TABLES_PATH)

    def test_commented_and_empty_lines_skipped(self):
        """Ensure that lines starting with ; (semicolon)
        which are comments, and empty lines are skipped when loading content of the table.
        Note that this works only for files with Unix line endings (LF)
        `test_crlf_not_skipped_for_empty_lines`  demonstrates
        the incorrect behaviour for files with Windows line endings.
        """
        table_entries_before_file_load = set(self.translator.table.keys())
        self.translator.load_file("empty_lines_and_comments.table")
        self.assertEqual(
            table_entries_before_file_load,
            set(self.translator.table.keys())
        )

    def test_crlf_not_skipped_for_empty_lines(self):
        """Demonstrates that empty lines in files with Windows line endings are not ignored when reading tables."""
        table_entries_before_file_load = set(self.translator.table.keys())
        self.translator.load_file("empty_lines_and_comments_crlf.table")
        keys_after_loading_table = set(self.translator.table.keys())
        self.assertEqual(
            (keys_after_loading_table- table_entries_before_file_load),
            set((u"\r\n",))
        )
        self.assertEqual(self.translator.table["\r\n"], "")

    def test_single_word_entries_added(self):
        """Verify the behaviour for adding entries where the translation is a single word."""
        self.translator.load_file("single_word.table")
        self.assertEqual(self.translator.table[u"+"], u"plus")

    def test_multiple_word_entries_added(self):
        """Verify the behaviour for adding entries where the translation consists of multiple words."""
        self.translator.load_file("multi_word.table")
        self.assertEqual(self.translator.table[u"\\pm"], u"plus or minus")

    def test_multiple_spaces_in_table_entry(self):
        """Verify the behaviour for adding entries where there are multiple spaces in the translation."""
        self.translator.load_file("multiple_spaces.table")
        self.assertEqual(
            self.translator.table[u"<"],
            u' "k '  # Space before and after
            )

    def test_all_files_from_file_list_loaded(self):
        """Ensure that entries from all added files are loaded."""
        self.translator.files.append("multi_word.table")
        self.translator.files.append("single_word.table")
        self.translator.load_files()
        self.assertEqual(self.translator.table[u"+"], u"plus")
        self.assertEqual(self.translator.table[u"\\pm"], u"plus or minus")
