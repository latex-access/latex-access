import unittest
from latex_access.matrix_processor import matrix

class TestMatrixProcessor(unittest.TestCase):
    def setUp(self):
        """Creates a matrix used for testing."""
        self.matrix = matrix()
        self.matrix.tex_init(r'1&2\\3&4')

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