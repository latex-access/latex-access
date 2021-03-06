import unittest
from latex_access.matrix_processor import matrix

class TestMatrixProcessor(unittest.TestCase):
    def setUp(self):
        """Creates a matrix used for testing."""
        self.matrix = matrix()
        self.matrix.tex_init(r'1&2\\3&4')

    def test_matrix_constructor(self):
        """Tests that instances of matrix class have default values."""
        self.matrix = matrix()
        self.assertEqual(self.matrix.elements, [])
        self.assertEqual(self.matrix.rows, 0)
        self.assertEqual(self.matrix.columns, 0)

    def test_matrix_creation(self):
        """Tests that matrix was properly created by fetching its attributes"""
        self.assertEqual(self.matrix.elements, [['1', '2'], ['3', '4']])
        self.assertEqual(self.matrix.rows, 2)
        self.assertEqual(self.matrix.columns, 2)

    def test_get_cell(self):
        """Tests single cell retrieval."""
        self.assertEqual(self.matrix.get_cell(2, 2), '4')

    def test_get_row(self):
        """Tests single row retrieval."""
        self.assertEqual(self.matrix.get_row(1, '&'), '1&2')

    def test_get_col(self):
        """Tests single column retrieval"""
        self.assertEqual(self.matrix.get_col(1, '&'), '1&3&')

    def test_initialised_stats(self):
        """Tests that statistics about matrix are properly initialised."""
        self.assertEqual(self.matrix.initialisedStats(), 'Initialised 2 by 2 matrix')

    def test_tex_init(self):
        """Tests that tex_init returns the statistics provided by initialisedStats."""
        self.assertEqual(self.matrix.tex_init(r'1&2\\3&4'), 'Initialised 2 by 2 matrix')

    def test_malformed_matrix(self):
        """Tests that malformed matrix is properly handled."""
        self.assertEqual('Rows 1 and 2 have a different number of columns, error.', self.matrix.tex_init(r'1&2\\3'))
        self.assertEqual(self.matrix.elements, [])
        self.assertEqual(self.matrix.rows, 0)
        self.assertEqual(self.matrix.columns, 0)

    def test_blank_row(self):
        """Tests that blank row is properly deleted."""
        self.matrix.tex_init(r'1&2\\3&4\\')
        self.assertEqual(self.matrix.rows, 2)

    def test_tex_init_resets_members(self):
        """Tests that `tex_init` clears instance members before parsing
        the provided string.
        """
        sampleMatrix = matrix()
        sampleMatrix.rows = -13
        sampleMatrix.columns = -22
        sampleMatrix.elements = "xyz"
        sampleMatrix.tex_init(r'1&2\\3&4')
        self.assertEqual(sampleMatrix.rows, 2)
        self.assertEqual(sampleMatrix.columns, 2)
        self.assertEqual(sampleMatrix.elements, [['1', '2'], ['3', '4']])
