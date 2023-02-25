# -*- coding: utf8 -*-
import unittest
from parameterized import parameterized
try:
    from latex_access import hungarian_speech
except ImportError:
    failed_to_import = True
else:
    failed_to_import = False


@unittest.skipIf(failed_to_import, "Hungarian speech module is incompatible with current version of python.")
class TestHungarianSpeech(unittest.TestCase):
    def setUp(self):
        """Creates a speech instance."""
        self.speech = hungarian_speech.speech()

    def test_adding_file(self):
        """Tests adding of speechtable file."""
        self.assertEqual(self.speech.files, ['speech.table'])

    def test_adding_to_table(self):
        """Tests adding of new elements to speech table."""
        self.assertEqual(self.speech.table["\\binom"], self.speech.binom)
        self.assertEqual(self.speech.table["\\bf"], ("<bold>","</bold>"))
        self.assertEquals(self.speech.table["\\bar"], ("",u" felülvonás "))

    def test_attributes(self):
        """Tests creation of attributes."""
        self.assertFalse(self.speech.handleBracketsAsPrime)
        self.assertFalse(self.speech.abbrev_first_root_arg)
        self.assertEqual(self.speech.space, ' ')

    def test_correct(self):
        """Tests replacing groups of letters."""
        self.assertEqual(self.speech.correct('yx'), 'y x')
        self.assertEqual(self.speech.correct('yy'), 'y y')
        self.assertEqual(self.speech.correct('imx'), 'i m x')

    def test_lowerSuffixParse(self):
        self.assertEqual(self.speech.lowerSuffixParse(''), (u'-tól '))
        self.assertEqual(self.speech.lowerSuffixParse(u'egyenlő 0'), (u"egyenlő 0-tól "))
        self.assertEqual(self.speech.lowerSuffixParse('0'), (u' nullától '))
        self.assertEqual(self.speech.lowerSuffixParse('1'), (u'1-től '))
        self.assertEqual(self.speech.lowerSuffixParse('3'), (u'3-tól '))
        self.assertEqual(self.speech.lowerSuffixParse('10A'), (u'10ától '))
        self.assertEqual(self.speech.lowerSuffixParse('1A '), (u'1ától '))
        self.assertEqual(self.speech.lowerSuffixParse('z'), (u'z-től '))

    def test_upperSuffixParse(self):
        self.assertEqual(self.speech.upperSuffixParse('1A'), (u'1áig '))
        self.assertEqual(self.speech.upperSuffixParse('1a '), (u'1áig '))
        self.assertEqual(self.speech.upperSuffixParse('x'), (u'x-ig '))

    def test_super(self):
        """Tests translation of superscripts."""
        self.assertEqual(self.speech.super('2', 0), (u' négyzet ', 1))
        self.assertEqual(self.speech.super('3', 0), (u' köb ', 1))
        self.assertEqual(self.speech.super('4', 0), (u' ad 4, ', 1))
        self.assertEqual(self.speech.super('{-4}', 0), (u' ad  minus 4, ', 4))
        self.assertEqual(self.speech.super('\\prime', 0), (u' vessző ', 6))
        self.assertEqual(self.speech.super('{\\prime\\prime}', 0), (u' két vessző ', 14))
        self.assertEqual(self.speech.super('\ppprime', 0), (u' 3 vessző ', 8))
        self.assertEqual(self.speech.super('{10}', 0), (u' ad <sup> 10 </sup>', 4))

    def test_sqrt(self):
        """Tests translations of roots."""
        self.assertEqual(self.speech.sqrt('2', 0), (u' gyök 2', 1))
        self.assertEqual(self.speech.sqrt('{2x}', 0), (u' gyök alatt, 2x, gyök zár', 4))

    def test_frac(self):
        """Tests translations of fractions."""
        self.assertEqual(self.speech.frac('{1}{2}', 0), ('1 ketted ', 6))
        self.assertEqual(self.speech.frac('{x}{y}', 0), (' x per y ', 6))
        self.assertEqual(self.speech.frac('{2x}{3y}', 0), (u' tört, 2x per 3y, tört zár ', 8))

    @parameterized.expand([(hungarian_speech.speech.integral, u"integrál"), (hungarian_speech.speech.dbintegral, u"kettős integrál"),
    (hungarian_speech.speech.ddintegral, u"hármas integrál"), (hungarian_speech.speech.ointegral, u"hurokintegrál")])
    def test_integrals(self, function, result):
        """Tests translations of integrals."""
        self.assertEqual(function(self.speech, '_{2}^{4}', 0), (u'%s 2-től 4-ig ' % (result), 8))
        self.assertEqual(function(self.speech, '_{2}', 0), (u'%s <sub>2</sub>' % (result), 4))

    @parameterized.expand([(hungarian_speech.speech.sum, u"szumma"), (hungarian_speech.speech.prod, u"produktum"),
    (hungarian_speech.speech.union, u"unió"), (hungarian_speech.speech.cap, u"metszet")])
    def test_other_functions(self, function, result):
        """Tests translatios of sums, products, unions and intersections."""
        self.assertEqual(function(self.speech, '_{2}^{4}', 0), (u' %s 2-től 4-ig ' % (result), 8))
        self.assertEqual(function(self.speech, '_{2}', 0), (' %s <sub>2</sub>' % (result), 4))

    def test_tag(self):
        """Tests translations of tags."""
        self.assertEqual(self.speech.tag('{10}', 0), (' tag left paren 10 right paren ', 4))

    def test_ang(self):
        """Tests translations of angles."""
        self.assertEqual(self.speech.ang('{30}', 0), ('30 degrees', 4))
        self.assertEqual(self.speech.ang('{45;10}', 0), ('45 degrees 10 minutes', 7))
        self.assertEqual(self.speech.ang('{60;15;20}', 0), ('60 degrees 15 minutes 20 seconds', 10))

    def test_log(self):
        """Tests translations of logarithms."""
        self.assertEqual(self.speech.log(' 5', 0), (' log ', 1))
        self.assertEqual(self.speech.log('_{2}8', 0), (u'2 alapú logaritmus ', 4))

    def test_underline(self):
        """Tests translations of overlines."""
        self.assertEqual(self.speech.underline('x', 0), ('x', 1))

    def test_overline(self):
        """Tests translations of overlines."""
        self.assertEqual(self.speech.overline('x', 0), (u'x felülvonás ', 1))
        self.assertEqual(self.speech.overline('{xy}', 0), (u' felülvonás x y felülvonás zár ', 4))

    def test_tilde(self):
        """Tests translations of tildes."""
        self.assertEqual(self.speech.tilde('x', 0), (u'x hullám ', 1))
        self.assertEqual(self.speech.tilde('{xy}', 0), (u' hullám x y hullám zár ', 4))

    def test_hat(self):
        """Tests translations of hats."""
        self.assertEqual(self.speech.hat('x', 0), (u'x kalap ', 1))
        self.assertEqual(self.speech.hat('{xy}', 0), (u' kalap x y kalap zár ', 4))

    def test_vect(self):
        """Tests translations of vectors."""
        self.assertEqual(self.speech.vect('a', 0), (u'á vektor ', 1))
        self.assertEqual(self.speech.vect('{xy}', 0), (u' vektor, x y, vektor zár ', 4))

    def test_binom(self):
        """Tests translations of binomials."""
        self.assertEqual(self.speech.binom('{n}{k}', 0), (' n alatt k ', 6))
        self.assertEqual(self.speech.binom('{n+1}{k+1}', 0), (u' binom, n plus 1 alatt k plus 1, binom zár ', 10))
