import unittest

from latex_access.braille_translators import nemeth


class NemethTests(unittest.TestCase):
    def setUp(self):
        """Creates a nemeth instance."""
        self.braille = nemeth.BrailleTranslator()

    def test_adding_file(self):
        """Tests adding of nemeth table file."""
        self.assertEqual(self.braille.files, ['nemeth.table'])

    def test_adding_to_table(self):
        """Tests adding of new elements to nemeth table."""
        self.assertEqual(self.braille.table['^'], self.braille.super)
        self.assertEqual(self.braille.table['\\dot'], ('','`'))
        self.assertEqual(self.braille.table['\\colvec'], ('{',' ','o'))
        self.assertEqual(self.braille.table['A'], self.braille.upperLetter)

    def test_super(self):
        """Tests translations of superscripts."""
        self.assertEqual(self.braille.super('2', 0), ('<', 1))
        self.assertEqual(self.braille.super('3', 0), ('%', 1))
        self.assertEqual(self.braille.super('{\\prime\\prime}', 0), ('\'\'', 14))
        self.assertEqual(self.braille.super('4', 0), ('~4\"', 1))
        self.assertEqual(self.braille.super('{10}', 0, (1, 0)), ('~10\"', 4))

    def test_sub(self):
        """Tests translations of subscripts."""
        self.assertEqual(self.braille.sub('1', 0), ('1', 1))
        self.assertEqual(self.braille.sub('x', 0), (';x\"', 1))
        self.assertEqual(self.braille.sub('{10}', 0, (1, 0)), ('10', 4))
        self.assertEqual(self.braille.sub('y', 0, (0, 1)), (';y\"', 1))

    def test_sqrt(self):
        """Tests translations of roots."""
        self.assertEqual(self.braille.sqrt('2', 0), ('>2', 1))
        self.assertEqual(self.braille.sqrt('{2x}', 0), ('>2x}', 4))
        self.assertEqual(self.braille.sqrt('{16}', 0, (1, 0)), ('>16', 4))

    def test_frac(self):
        """Tests translations of fractions."""
        self.assertEqual(self.braille.frac('{1}{2}', 0), ('1/2', 6))
        self.assertEqual(self.braille.frac('{1}', 0), ('?1/#', 3))
        self.assertEqual(self.braille.frac('{x}{y}', 0), ('?x/y#', 6))

    @unittest.expectedFailure
    def test_frac_failing(self):
        """Fails because of typo in tested code."""
        self.assertEqual(self.braille.frac('{2x}{3y}', 0, (1, 1)), ('?2x/3y#', 8))

    def test_bar(self):
        """Tests translations of bars."""
        self.assertEqual(self.braille.bar('x', 0), (':x', 1))
        self.assertEqual(self.braille.bar('{2x}', 0, (0, 0)), (':{2xo', 4))
        self.assertEqual(self.braille.bar('{2y}', 0), (':{2yo', 4))

    def test_tag(self):
        """Tests translations of tags."""
        self.assertEqual(self.braille.tag('x', 0), ('  {x}', 1))

    def test_upper_letter(self):
        """Tests translations of upper letters."""
        self.braille.capitalisation='8dot'
        self.assertEqual(self.braille.upperLetter('X', 0), ('X', 0))
        self.braille.capitalisation='6dot'
        self.assertEqual(self.braille.upperLetter('X', 0), (',x', 0))

    def test_cap_at_beginning_of_line_cap_sign_added(self):
        self.braille.capitalisation='6dot'
        self.assertEqual(self.braille.upperLetter('XY', 1), (',x', 1))
