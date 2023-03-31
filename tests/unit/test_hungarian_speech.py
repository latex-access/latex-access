# -*- coding: utf8 -*-
import unittest
from parameterized import parameterized
from .hungarian_speech_wrapper import hungarian_speech
try:
    hungarian_speech.speech()
except AttributeError:
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
        self.assertEqual(self.speech.table["\\bar"], ("",u" felülvonás "))

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
        self.assertEqual(self.speech.super(r'\ppprime', 0), (u' 3 vessző ', 8))
        self.assertEqual(self.speech.super('{10}', 0), (u' ad <sup> 10 </sup>', 4))

    def test_sqrt(self):
        """Tests translations of roots."""
        self.assertEqual(self.speech.sqrt('2', 0), (u' gyök 2', 1))
        self.assertEqual(self.speech.sqrt('{2x}', 0), (u' gyök alatt, 2x, gyök zár', 4))
        self.assertEqual(self.speech.sqrt('\\sqrt[2]{16}', 5), (u'2.  gyök alatt, 16, gyök zár ', 12))
        self.assertEqual(self.speech.sqrt('\\sqrt[3]{27}', 5), (u' köb  gyök alatt, 27, gyök zár ', 12))
        self.assertEqual(self.speech.sqrt('\\sqrt[N]{27}', 5), (u'n-edik  gyök alatt, 27, gyök zár ', 12))
        self.assertEqual(self.speech.sqrt('\\sqrt[x]{27}', 5), (u'x-edik  gyök alatt, 27, gyök zár ', 12))
        self.assertEqual(self.speech.sqrt('\\sqrt[x]{7}', 5), (u'x-edik  gyök 7', 11))

    def test_frac(self):
        """Tests translations of fractions."""
        self.assertEqual(self.speech.frac('{1}{2}', 0), ('1 ketted ', 6))
        self.assertEqual(self.speech.frac('{x}{y}', 0), (' x per y ', 6))
        self.assertEqual(self.speech.frac('{2x}{3y}', 0), (u' tört, 2x per 3y, tört zár ', 8))
        self.assertEqual(self.speech.frac('{1}', 0), (u' tört, 1 per , tört zár ', 3))
        self.assertEqual(self.speech.frac(r'\frac{3}{2}', 5), ('3 ketted ', 11))

    def test_dsfrac(self):
        """Unclear what `\\dsfrac`` does"""
        self.assertEqual(self.speech.dsfrac("{1}", 0), (u" tört 1 tört zár ", 3))

    @parameterized.expand([(hungarian_speech.speech.integral, u"integrál"), (hungarian_speech.speech.dbintegral, u"kettős integrál"),
    (hungarian_speech.speech.ddintegral, u"hármas integrál"), (hungarian_speech.speech.ointegral, u"hurokintegrál")])
    def test_integrals(self, function, result):
        """Tests translations of integrals."""
        self.assertEqual(function(self.speech, '_{2}^{4}', 0), (u'%s 2-től 4-ig ' % (result), 8))
        self.assertEqual(function(self.speech, '_{2}', 0), (u'%s <sub>2</sub>' % (result), 4))
        self.assertEqual(function(self.speech, '', 0), (u"{} ".format(result), 0))

    @parameterized.expand([(hungarian_speech.speech.sum, u"szumma"), (hungarian_speech.speech.prod, u"produktum"),
    (hungarian_speech.speech.union, u"unió"), (hungarian_speech.speech.cap, u"metszet")])
    def test_other_functions(self, function, result):
        """Tests translatios of sums, products, unions and intersections."""
        self.assertEqual(function(self.speech, '_{2}^{4}', 0), (u' %s 2-től 4-ig ' % (result), 8))
        self.assertEqual(function(self.speech, '_{2}', 0), (' %s <sub>2</sub>' % (result), 4))
        self.assertEqual(function(self.speech, '', 0), (u" {} ".format(result), 0))

    def test_tag(self):
        """Tests translations of tags."""
        self.assertEqual(self.speech.tag('{10}', 0), (' tag left paren 10 right paren ', 4))

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
        self.assertEqual(self.speech.binom('{n+1}', 0), (u' binom, n plus 1 alatt , binom zár ', 5))

    def test_angle_no_coordinates(self):
        """Check translation for angles without degrees, minutes and seconds."""
        self.assertEqual(self.speech.ang(r"\ang{30}", 4), ("30 degrees", 8))

    def test_angle_minutes_present(self):
        """Check translation for angles where minutes are present.

        Note that behaviour of ang method in this case seems incorrect
        the trailing semicolon after minutes
        causes unnecesary seconds to be added.
        """
        self.assertEqual(
            self.speech.ang(r"\ang{52;58.110;}", 4),
            ("52 degrees 58.110 minutes  seconds", 16)
        )

    def test_angle_minutes_present_no_trailing_semicolon(self):
        """Check translation for angles where minutes are present."""
        self.assertEqual(
            self.speech.ang(r"\ang{52;58.110}", 4),
            ("52 degrees 58.110 minutes", 15)
        )

    def test_angle_minutes_and_seconds_present(self):
        """Check translation for angles with both minutes and seconds."""
        self.assertEqual(
            self.speech.ang(r"\ang{52;58;13}", 4),
            ("52 degrees 58 minutes 13 seconds", 14)
        )

    def test_angle_minutes_and_seconds_present_trailing_semicolon(self):
        """Check translation for angles with both minutes and seconds + semicolon at the end."""
        self.assertEqual(
            self.speech.ang(r"\ang{52;58;13;}", 4),
            ("52 degrees 58 minutes 13 seconds ", 15)
        )

    def test_angle_semicolon_after_seconds_kept_verbatim(self):
        """Check translation for angles with minutes, seconds and additional semicolon at the end.

        Note that it is unclear how exactly such coordinate looks when compiled.
        It is also unclear if keeping the part after semicolon
        which ends seconds is the best translation possible.
        """
        self.assertEqual(
            self.speech.ang(r"\ang{52;58;13;22;}", 4),
            ("52 degrees 58 minutes 13 seconds 22;", 18)
        )

    def test_translation_primes_handle_brackets_as_prime_enabled(self):
        """Test with `handleBracketsAsPrime` set to `True`.

        Note that regardless of how this is set the output is the same.
        Note also that the condition which checks for `handleBracketsAsPrime`
        is implemented incorrectly, since it checks for the value of this variable
        in an `elif` block which never executes
        due to the fact that we check for the presence of primes in the argument above.
        """
        self.speech.handleBracketsAsPrime = True
        self.assertEqual(self.speech.super('\\prime', 0), (u' vessző ', 6))

    def test_roots_first_arg_abbreviated(self):
        """Test translation of roots with `abbrev_first_root_arg` set to `True`."""
        self.speech.abbrev_first_root_arg = True
        self.assertEqual(
            self.speech.sqrt(r"\sqrt[2]{16}", 5),
            (u" ketted  gyök alatt, 16, gyök zár ", 12)
        )
        with self.assertRaises(ValueError):
            # The code under test tries to convert root of the degree
            # to int not only for integers,
            # but also for every single character string,
            # which obviously fails.
            self.assertEqual(
                self.speech.sqrt(r"\sqrt[x]{16}", 5),
                (u" ketted  gyök alatt, 16, gyök zár ", 12)
            )
        with self.assertRaises(ValueError):
            self.assertEqual(
                self.speech.sqrt(r"\sqrt[n]{27}", 5),
                (u" harmad  gyök alatt, 27, gyök zár ", 12)
            )
        self.assertEqual(
            self.speech.sqrt(r"\sqrt[3]{27}", 5),
            (u" harmad  gyök alatt, 27, gyök zár ", 12)
        )
        self.assertEqual(
            self.speech.sqrt(r"\sqrt[]{2}", 5),
            (u".  gyök 2", 10)
        )

    def test_absolute_value_translation(self):
        """Set of tests for `norma` method.

While it is not used currently
it was written to handle absolute values written using | (pipe) character.
        """
        self.assertEqual(self.speech.norma(" |-2", 1), (u" függőleges ", 2))
        self.assertEqual(self.speech.norma("|x|", 0), (u"x abszolút ", 3))
        self.assertEqual(
            self.speech.norma("|-2|", 0),
            (u" abszolút  minus 2 abszolút ", 4)
        )
