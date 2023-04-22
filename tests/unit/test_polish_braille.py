# -*- coding: utf8 -*-
import unittest
from latex_access import polish_braille


class TestPolishBraille(unittest.TestCase):
    def setUp(self):
        """Creates a braille instance."""
        self.braille = polish_braille.Braille()

    def test_adding_file(self):
        """Tests adding of braille table file."""
        self.assertEqual(self.braille.files, ['polish_braille.table'])

    def test_adding_to_table(self):
        """Tests adding of new elements to braille table."""
        self.assertEqual(self.braille.table['^'], self.braille.super)
        self.assertEqual(self.braille.table['\\dot'], ('','`'))
        self.assertEqual(self.braille.table['\\abs'], (u'⠈l',u'⠸'))
        self.assertEqual(self.braille.table['0'], self.braille.numbers)
        self.assertEqual(self.braille.table['A'], self.braille.upperLetter)
        self.assertEqual(self.braille.table['a'], self.braille.lowerLetter)

    def test_lowered_digits(self):
        """Tests creation of lowered digits dictionary."""
        self.assertEqual(self.braille.lowered_digits['L0'], u'⠴')

    def test_before(self):
        """Tests before method."""
        self.braille.before()
        self.assertEqual(self.braille.lastnumber, -1)

    def test_super(self):
        """Tests translations of superscripts."""
        self.assertEqual(self.braille.super('2', 0), (u"⠬⠆", 1))
        self.assertEqual(self.braille.super('3', 0), (u"⠬⠒", 1))
        self.assertEqual(self.braille.super('\\circ', 0), (u'⠴', 5))
        self.assertEqual(self.braille.super('{\\prime\\prime}', 0), (u'⠔⠔', 14))
        self.assertEqual(self.braille.super('{10}', 0), (u'ó⠂⠴', 4))
        self.assertEqual(self.braille.super('{10}', 0, (0, 0)), (u'ó⠂⠴', 4))
        self.assertEqual(self.braille.super('{-1}', 0), (u'ó-⠂', 4))
        self.assertEqual(self.braille.super('{2a}', 0), (u'ó#b⠠a', 4))
        self.assertEqual(self.braille.super('{-1}', 0, (0, 0)), (u'ó-⠂', 4))
        self.assertEqual(self.braille.super('{-x}', 0, (0, 0)), (u'ó-⠠x', 4))
        self.assertEqual(self.braille.super('{-\\frac{1}{2}}', 0, (0, 0)), (u'ó-1⠆', 14))
        self.assertEqual(self.braille.super('{ a}', 0, (0, 0)), (u'ó⠠ ⠠a', 4))
        self.assertEqual(self.braille.super('{\\frac{1}{2}}', 0, (0, 0)), (u'ó1⠆', 13))

    def test_sub(self):
        """Tests translations of subscripts."""
        self.assertEqual(self.braille.sub('1', 0), (u'ą⠂', 1))
        self.assertEqual(self.braille.sub('x', 0), (u'ą⠠x', 1))
        self.assertEqual(self.braille.sub('{xy}', 0), (u'ąxyą', 4))
        self.assertEqual(self.braille.sub('{xy}', 0, (0, 0)), (u'ąxyą', 4))

    def test_sqrt(self):
        """Tests translations of roots."""
        self.assertEqual(self.braille.sqrt('[2]{4}', 0), (u'⠌⠆ć#d', 6))
        self.assertEqual(self.braille.sqrt('[3]{9}', 0), (u'⠌⠒ć#i', 6))
        self.assertEqual(self.braille.sqrt('[x]{4}', 0), (u'⠌⠭ć#d', 6))
        self.assertEqual(self.braille.sqrt('[N]{4}', 0), (u'⠌⠝ć#d', 6))
        self.assertEqual(self.braille.sqrt('[4]{16}', 0), (u'⠌⠲ć#af', 7))
        self.assertEqual(self.braille.sqrt('[3]{x+1}', 0), (u'⠌⠒ćx+#aę', 8))
        self.assertEqual(self.braille.sqrt('[]{2}', 0), (u'ć#b', 5))
        self.assertEqual(self.braille.sqrt('x', 0), (u'ćx', 1))
        self.assertEqual(self.braille.sqrt('{2x}', 0), (u'ć#bxę', 4))

    def test_frac(self):
        """Tests translations of fractions."""
        self.assertEqual(self.braille.frac('{1}{2}', 0), (u'1⠆', 6))
        self.assertEqual(self.braille.frac('{1}', 0), (u'⠆#a⠳⠰', 3))
        self.assertEqual(self.braille.frac('{x}{y}', 0), (u'⠆⠠x⠳⠠y⠰', 6))
        self.assertEqual(self.braille.frac('2\\frac{1}{2}', 6), (u'#a⠆', 12))
        self.assertEqual(self.braille.frac('2\\frac{11}{2}', 6), (u'#aa⠆', 13))
        self.assertEqual(self.braille.frac('{-1}{2}', 0), (u'⠆-#a⠳#b⠰', 7))
        self.assertEqual(self.braille.frac('{2x}{3y}', 0, (1, 1)), (u'⠆#bx⠳#cy⠰', 8))
        self.assertEqual(
            self.braille.frac('2\\frac{-1}{2}', 6),
            (u'⠆-#a⠳#b⠰', 13)
        )

    def test_bar(self):
        """Tests translations of bars."""
        self.assertEqual(self.braille.bar('1', 0), (':1', 1))
        self.assertEqual(self.braille.bar('{11}', 0), (':{#aao', 4))
        self.assertEqual(self.braille.bar('{11}', 0, (0, 0)), (':{#aao', 4))

    def test_log(self):
        """Tests translations of logarithms."""
        self.assertEqual(self.braille.log('4', 0), (u'⠫l', 0))
        self.assertEqual(self.braille.log('_{2}8', 0), (u'⠌⠆⠫l', 4))
        self.assertEqual(self.braille.log('_{a}8', 0), (u'⠌⠠a⠫l', 4))

    def test_pmod(self):
        """Tests translations of modules."""
        self.assertEqual(self.braille.pmod('1', 0), (u'⠈l#a⠸', 1))
        self.assertEqual(self.braille.pmod('1', 0, (0, 0)), (u'⠈l#a⠸', 1))
        self.assertEqual(self.braille.pmod('x', 0), (u'⠈l⠠x⠸', 1))
        self.assertEqual(self.braille.pmod('x', 0, (1, 1)), (u'⠈l⠠x⠸', 1))

    def test_letterSign(self):
        """Tests translations of letter sign."""
        self.braille.lastnumber = 0
        self.assertFalse(self.braille.letterSign(' A ', 1))
        self.assertTrue(self.braille.letterSign('A', 0))
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
        self.assertEqual(self.braille.lowerLetter('b', 0), (u'⠠b', 0))
        self.assertEqual(self.braille.lowerLetter('{b}', 2), (u'⠠b', 2))
        self.assertEqual(self.braille.lowerLetter('{ABc}', 4), (u'⠠c', 4))

    def test_upperLetter(self):
        """Tests translations of upper letters."""
        self.braille.lastnumber = 1
        self.braille.capitalisation = '8dot'
        self.assertEqual(self.braille.upperLetter('A', 0), ('A', 0))
        self.braille.capitalisation = '6dot'
        self.assertEqual(self.braille.upperLetter('A', 0), (u'⠨a', 0))
        self.assertEqual(self.braille.upperLetter('{AB}', 2), (u'⠨⠨a', 2))
        self.assertEqual(self.braille.upperLetter('{A}', 1), (u'⠨⠨{', 1))
        self.assertEqual(self.braille.upperLetter('B', 0), (u'⠨b', 0))
        self.assertEqual(self.braille.upperLetter('{ABC}', 3), ('b', 3))
        self.assertEqual(self.braille.upperLetter("A", 1), (u"⠨a", 1))

    def test_numbers(self):
        """Tests translations of numbers."""
        self.braille.lastnumber = -1
        self.assertEqual(self.braille.numbers('0', 0), ('#j', 0))
        self.braille.lastnumber = 1
        self.assertEqual(self.braille.numbers('{11}', 2), ('a', 2))

    def test_comma_in_text(self):
        """When comma (,) occurs in the text,
        position of the last number should not be updated."""
        self.braille.before()  # Set lastnumber to default
        self.assertEqual(
            self.braille.comma("There is a comma, here", 15),
            (",", 15)
        )
        self.assertEqual(self.braille.lastnumber, -1)

    def test_comma_between_numbers(self):
        """When comma (,) occurs between digits,
        position of lastnumber should be set to the position of the comma.
        While this is admittedly a hack
        it ensures that we would not add a number sign in cases where comma is used as a decimal separator.
        """
        self.braille.lastnumber = 0
        self.assertEqual(self.braille.comma("1,25", 1), (",", 1))
        self.assertEqual(self.braille.lastnumber, 1)
