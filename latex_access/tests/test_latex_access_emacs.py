import os
import unittest
import pickle
import tempfile
try:
    from latex_access.latex_access_emacs import *
except SyntaxError:
    incompatible = True
else:
    incompatible = False

HERE = os.path.abspath(os.path.dirname(__file__))


@unittest.skipIf(incompatible, "This module is not compatible with version 3 of python.")
class TestsLatexAccessEmacs(unittest.TestCase):
    def test_transbrl(self):
        """Tests translations to braille."""
        activateSettings()
        self.assertEqual(transbrl('2^2'), '2<')

    def test_transsp(self):
        """Tests translations to speech."""
        self.assertEqual(transsp('2^2'), ('2 squared '))

    def test_toggle_dolars_speech(self):
        """Tests toggling of dollar signs."""
        self.assertTrue(toggle_dollars_speech())

    def test_preprocessor_add(self):
        """Tests adding entries to a preprocessor."""
        preprocessor_add('foo', 'args', '1#2#3')
        self.assertEqual(p.table['foo'], ['args', '1', 2, '', 3, ''])

    def test_preprocessor_from_string(self):
        """Tests adding entries with newcommand."""
        self.assertEqual(preprocessor_from_string('2^2'), None)

    def test_write(self):
        """Tests that preprocessor entries can be saved to a file."""
        with tempfile.NamedTemporaryFile(delete=False) as file:
            p.table['foo'] = 'bar'
            preprocessor_write(file.name)
        with open(file.name, "r") as f:
            temp_table = pickle.load(f)
        self.assertEqual(temp_table['foo'], 'bar')

    def test_read(self):
        """Tests that preprocessor entries can be read from a file."""
        preprocessor_read(os.path.join(HERE, 'preprocessor_file'))
        self.assertEqual(p.table['foo'], 'bar')

    def test_BuildHeaderString(self):
        """Tests header string building for tables."""
        self.assertEqual(BuildHeaderString('1 & 2 & 3'), ['1 ', ' 2 ', ' 3'])

    def test_WhereAmI(self):
        """Tests retrieving locations in tables."""
        self.assertEqual(WhereAmI('4&5', ['1','2','3'], '1&2&3\\\\4&5&6'), 'focus is in column 2 at location B 2')

    def test_GetTableTopRow(self):
        """Tests retrieving top row in tables."""
        self.assertEqual(GetTableTopRow('1&2&3\\\\4&5&6'), '1&2&3')

    def test_GetTableCurrentRow(self):
        """Tests retrieving current row in tables."""
        self.assertEqual(GetTableCurrentRow('1&2&3\\\\4&5&6'), '\\4&5&6')


@unittest.skipIf(incompatible, "This module is not compatible with version 3 of python.")
class TestsLatexAccessEmacsMatrix(unittest.TestCase):
    def setUp(self):
        """Initializes a matrix."""
        matrixInit('1&2\\\\3&4')

    def test_matrixUp(self):
        """Tests moving up within a matrix."""
        self.assertEqual(matrixUp(), 'top of matrix')
        matrix.row = 2
        self.assertEqual(matrixUp(), '1')

    def test_matrixDown(self):
        """Tests moving down within a matrix."""
        self.assertEqual(matrixDown(), '3')
        self.assertEqual(matrixDown(), 'bottom of matrix')

    def test_matrixLeft(self):
        """Tests moving left within a matrix."""
        self.assertEqual(matrixLeft(), 'first cell')
        matrix.column = 2
        self.assertEqual(matrixLeft(), '1')

    def test_matrixRight(self):
        """Tests moving right within a matrix."""
        self.assertEqual(matrixRight(), '2')
        self.assertEqual(matrixRight(), 'last cell')

    def test_matrixGoto(self):
        """Tests moving to a specified cell."""
        self.assertEqual(matrixGoto(2, 2), '4')
        self.assertEqual(matrixGoto(3, 3), 'invalid cell')

    def test_matrixInit(self):
        """Tests that matrix is properly created."""
        self.assertEqual(matrix.row, 1)
        self.assertEqual(matrix.column, 1)