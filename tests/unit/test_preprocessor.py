import os
import unittest
import tempfile

try:
    from latex_access import preprocessor
except ImportError:
    incompatible = True
else:
    incompatible = False

HERE = os.path.abspath(os.path.dirname(__file__))


@unittest.skipIf(incompatible, "This module is not compatible with version 3 of Python.")
class TestPreprocessor(unittest.TestCase):
    def setUp(self):
        """Creates a preprocessor instance."""
        self.preprocessor = preprocessor.preprocessor()

    def test_table(self):
        """Tests creation of table attribute."""
        self.assertEqual(self.preprocessor.table, {})

    def test_add(self):
        """Test adding command to table."""
        self.preprocessor.add('foo', 'bar')
        self.assertEqual(self.preprocessor.table['foo'], ('bar'))

    def test_add_from_string(self):
        """Tests adding command with args."""
        self.preprocessor.add_from_string('foo', 'args', '1#2#3')
        self.assertEqual(self.preprocessor.table['foo'], ['args', '1', 2, '', 3, ''])

    def test_write(self):
        """Tests that preprocessor entries can be saved to a file."""
        with tempfile.NamedTemporaryFile(delete=False) as file:
            self.preprocessor.table['foo'] = 'bar'
            self.preprocessor.write(file.name)
        clean_preproc = preprocessor.preprocessor()
        clean_preproc.read(file.name)
        self.assertEqual(clean_preproc.table["foo"], "bar")

    def test_read(self):
        """Tests that preprocessor entries can be read from a file."""
        self.preprocessor.read(os.path.join(HERE, 'preprocessor_file'))
        self.assertEqual(self.preprocessor.table['foo'], 'bar')


@unittest.skipIf(incompatible, "This module is not compatible with version 3 of Python.")
class TestNewcommands(unittest.TestCase):
    def setUp(self):
        """Creates a newcommands instance."""
        self.newcommands = preprocessor.newcommands(preprocessor.preprocessor())

    def test_adding_to_table(self):
        """Tests adding of new elements to table."""
        self.assertEqual(self.newcommands.table['\\newcommand'], self.newcommands.newcommand)
        self.assertEqual(self.newcommands.table['\\renewcommand'], self.newcommands.newcommand)

    def test_preprocessor_attribute(self):
        """Tests that preprocessor attribute is created."""
        self.assertIsInstance(self.newcommands.preprocessor, preprocessor.preprocessor)

    def test_newcommand(self):
        self.assertEqual(self.newcommands.newcommand('\foo{1}', 5), ('', 6))
        self.assertEqual(self.newcommands.newcommand('\\foo[1]{2}', 0), ('', 10))
