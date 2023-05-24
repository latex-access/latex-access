import unittest

from latex_access.braille_translators import ueb


class UebTests(unittest.TestCase):
    def setUp(self):
        """Creates a ueb instance."""
        self.braille = ueb.BrailleTranslator()

    def test_adding_file(self):
        """Tests adding of nemeth table file."""
        self.assertEqual(self.braille.files, ['ueb.table'])

    def test_adding_to_table(self):
        """Tests adding of new elements to ueb table."""
        self.assertEqual(self.braille.table['$'], self.braille.uebDollar)
        self.assertEqual(self.braille.table['^'], self.braille.super)
        self.assertEqual(self.braille.table['\\dot'], ('','`'))
        self.assertEqual(self.braille.table['\\colvec'], ('{',' ','o'))
        self.assertEqual(self.braille.table['A'], self.braille.upperLetter)
        self.assertEqual(self.braille.table['a'], self.braille.lowerLetter)
        self.assertEqual(self.braille.table['0'], self.braille.numbers)

    def test_before(self):
        """Tests before method."""
        self.braille.before()
        self.assertEqual(self.braille.lastnumber, -1)

    def test_super(self):
        """Tests translations of superscripts."""
        self.assertEqual(self.braille.super('2', 0), (';9#b', 1))
        self.assertEqual(self.braille.super('2', 0, (1, 1)), (';9#b', 1))
        self.assertEqual(self.braille.lastnumber, 1)
        self.assertEqual(self.braille.super('{10}', 0), (';9<#aj>', 4))
        self.assertEqual(self.braille.super('{10}', 0, (0, 0)), (';9<#aj>', 4))
        self.assertEqual(self.braille.super('{-1}', 0), (u';9<"-#a>', 4))

    def test_sub(self):
        """Tests translations of subscripts."""
        self.assertEqual(self.braille.sub('2', 0), (';5#b', 1))
        self.assertEqual(self.braille.sub('2', 0, (1, 1)), (';5#b', 1))
        self.assertEqual(self.braille.lastnumber, 1)
        self.assertEqual(self.braille.sub('{10}', 0), (';5<#aj>', 4))
        self.assertEqual(self.braille.sub('{10}', 0, (0, 0)), (';5<#aj>', 4))
        self.assertEqual(self.braille.sub('{-1}', 0), (u';5<"-#a>', 4))

    def test_sqrt(self):
        """Tests translations of roots."""
        self.assertEqual(self.braille.sqrt('[2]{8}', 0), (';;%9#b#h+', 6))
        self.assertEqual(self.braille.sqrt('{3}', 0), ('%#c+', 3))
        self.assertEqual(self.braille.sqrt('{3}', 0, (1, 1)), ('%#c+', 3))

    def test_frac(self):
        """Tests translations of fractions."""
        self.assertEqual(self.braille.frac('{1}{2}', 0), ('#a/b', 6))
        self.assertEqual(self.braille.lastnumber, 6)
        self.assertEqual(self.braille.frac('{x}{y}', 0), (';(;x./;y;)', 6))
        self.assertEqual(self.braille.lastnumber, -1)

    @unittest.expectedFailure
    def test_frac_failing(self):
        """Fails because of typo in tested code."""
        self.assertEqual(self.braille.frac('{x}{y}', 0, (1, 1)), (';(;x./;y;)', 6))

    def test_bar(self):
        """Tests translations of bars."""
        self.assertEqual(self.braille.bar('x', 0), (':;x', 1))
        self.assertEqual(self.braille.bar('x', 0, (1, 1)), (':;x', 1))
        self.assertEqual(self.braille.bar('{2x}', 0, (0, 0)), (':{#bxo', 4))
        self.assertEqual(self.braille.bar('{2y}', 0), (':{#byo', 4))

    def test_tag(self):
        """Tests translations of tags."""
        self.assertEqual(self.braille.tag('x', 0), (' "<;x">', 1))
        self.assertEqual(self.braille.tag('x', 0, (1, 1)), ('"<;x">', 1))

    def test_uebDollar(self):
        """Tests translations of dollars."""
        self.braille.remove_dollars = False
        self.assertEqual(self.braille.uebDollar('%', 0), ('`s', 0))
        self.braille.remove_dollars = True
        self.assertEqual(self.braille.uebDollar('%', 0), ('', 0))

    def test_numbers(self):
        """Tests translations of numbers."""
        self.braille.lastnumber = -1
        self.assertEqual(self.braille.numbers('0', 0), ('#j', 0))
        self.braille.lastnumber = 1
        self.assertEqual(self.braille.numbers('{11}', 2), ('a', 2))

    def test_dot_in_the_middle_of_a_number(self):
        """Tests translations of dots between numbers."""
        # Simulate a case when dot is directly after a digit.
        self.braille.lastnumber = 0 
        self.assertEqual(self.braille.dot('1', 1), ('4', 1))
        # Ensure that last number is set to the starting position.
        self.assertEqual(self.braille.lastnumber, 1)

    def test_dot(self):
        """Tests translations of dots."""
        # We need to call `before` manually,
        # to set position of the last encountered number.
        # Normally this is taken care of by the base `translate` method.
        self.braille.before()
        self.assertEqual(self.braille.dot('1', 1), ('4', 1))
        # Ensure that start number remains unchanged.
        self.assertEqual(self.braille.lastnumber, -1)

    def test_comma_between_digits(self):
        """Tests translations of commas when they are between digits."""
        # Simulate a case when comma is directly after a digit.
        self.braille.lastnumber = 0
        self.assertEqual(self.braille.comma('1', 1), ('1', 1))
        # Ensure that last number is set to the starting position.
        self.assertEqual(self.braille.lastnumber, 1)

    def test_comma(self):
        """Tests translations of commas."""
        # We need to call `before` manually,
        # to set position of the last encountered number.
        # Normally this is taken care of by the base `translate` method.
        self.braille.before()
        self.assertEqual(self.braille.comma('1', 1), ('1', 1))
        # Ensure that start number remains unchanged.
        self.assertEqual(self.braille.lastnumber, -1)

    def test_letterSign(self):
        """Tests translations of letter sign."""
        self.braille.lastnumber = 0
        self.assertFalse(self.braille.letterSign('A', 0))
        self.assertTrue(self.braille.letterSign('B', 0))
        self.braille.lastnumber = 1
        self.assertTrue(self.braille.letterSign('{ B }', 2))
        self.assertTrue(self.braille.letterSign('{ }', 0))
        self.assertTrue(self.braille.letterSign('{,}', 2))
        self.assertTrue(self.braille.letterSign('{b,}', 1))
        self.assertTrue(self.braille.letterSign('{,b }', 2))
        self.assertTrue(self.braille.letterSign(' b', 1))

    def test_lowerLetter(self):
        """Tests translations of lower letters."""
        self.braille.lastnumber = 1
        self.assertEqual(self.braille.lowerLetter('b', 0), (';b', 0))
        self.assertEqual(self.braille.lowerLetter('{b}', 2), (';b', 2))
        self.assertEqual(self.braille.lowerLetter('{ABc}', 4), (',c', 4))

    def test_upperLetter(self):
        """Tests translations of upper letters."""
        self.braille.lastnumber = 1
        self.braille.capitalisation = '8dot'
        self.assertEqual(self.braille.upperLetter('A', 0), ('A', 0))
        self.braille.capitalisation = '6dot'
        self.assertEqual(self.braille.upperLetter('A', 0), (',a', 0))
        self.assertEqual(self.braille.upperLetter('{AB}', 2), (';,,a', 2))
        self.assertEqual(self.braille.upperLetter('{A}', 1), (',,{', 1))
        self.assertEqual(self.braille.upperLetter('B', 0), (';,b', 0))
        self.assertEqual(self.braille.upperLetter('{ABC}', 3), ('b', 3))