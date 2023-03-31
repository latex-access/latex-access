import unittest
from parameterized import parameterized
from latex_access import speech

try:
    test_speech = speech.speech()
    test_speech.sqrt('{2^2}', 0)
    incompatible = False
except AttributeError:
    incompatible = True

class TestSpeech(unittest.TestCase):

    def setUp(self):
        """Creates a speech instance."""
        self.speech = speech.speech()

    def test_adding_file(self):
        """Tests adding of speechtable file."""
        self.assertEqual(self.speech.files, ['speech.table'])

    def test_adding_to_table(self):
        """Tests adding of new elements to speech table."""
        self.assertEqual(self.speech.table['^'], self.speech.super)
        self.assertEqual(self.speech.table['\\int'], self.speech.integral)
        self.assertEqual(self.speech.table[r'\mathbb'], ("<bold>","</bold>"))

    def test_space(self):
        """Tests creation of space attribute."""
        self.assertEqual(self.speech.space, ' ')

    def test_super(self):
        """Tests translations of superscripts."""
        self.assertEqual(self.speech.super('2', 0), (' squared ', 1))
        self.assertEqual(self.speech.super('3', 0), (' cubed ', 1))
        self.assertEqual(self.speech.super(r'{\prime}', 0), (' prime ', 8))
        self.assertEqual(self.speech.super('{10}', 0), (' to the <sup> 10 </sup> ', 4))

    @unittest.skipIf(incompatible, "This test fails in Python 3 because of StringType attribute not available in Types module.")
    def test_complex_roots(self):
        """Tests translation of complex roots. Separated from other tests because it fails on Python 3."""
        self.assertEqual(self.speech.sqrt('{2^2}', 0), (' begin  root 2 squared  end root', 5))

    def test_frac(self):
        """Tests translations of fractions."""
        self.assertEqual(self.speech.frac('{1}{2}', 0), ('1 half ', 6))
        self.assertEqual(self.speech.frac('{1}', 0), (' begin frac 1 over  end frac ', 3))
        self.assertEqual(self.speech.frac('{3}{4}', 0), ('3 quarters ', 6))
        self.assertEqual(self.speech.frac('{x}{y}', 0), (' x over y ', 6))
        self.assertEqual(self.speech.frac('{5}{11}', 0), (' begin frac 5 over 11 end frac ', 7))

    def test_tag(self):
        """Tests translations of tags."""
        self.assertEqual(self.speech.tag('{10}', 0), (' tag left paren 10 right paren ', 4))

    def test_log(self):
        """Tests translations of logarithms."""
        self.assertEqual(self.speech.log(' 5', 0), (' log ', 1))
        self.assertEqual(self.speech.log('_{2}8', 0), (' log base 2 of ', 4))

    @parameterized.expand([(speech.speech.integral, "integral"), (speech.speech.iintegral, "double integral"), (speech.speech.iiintegral, "triple integral"),
    (speech.speech.sum, "sum"), (speech.speech.prod, "product"), (speech.speech.union, "union"), (speech.speech.intersection, "intersection")])
    def test_other_functions(self, function, result):
        """Tests different types of integrals as well as sums, products, unions and intersections.
        Because these functions are quite similar only one test is used which is parametrized to check all of them."""
        self.assertEqual(function(self.speech, '_{2}^{4}', 0), (' %s from 2 to 4 of ' % (result), 8))
        self.assertEqual(function(self.speech, '_{2}', 0), (' %s <sub>2</sub>' %(result), 4))
        self.assertEqual(function(self.speech, '', 0), (' %s ' %(result), 0))

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

    def test_simple_root_single_digit_num(self):
        """Check translation for a square root of two."""
        self.assertEqual(self.speech.sqrt(r"\sqrt{2}", 5), (" root 2", 8))

    def test_simple_root_multi_digit_num(self):
        """Check translation for a square root for number with multiple digits."""
        self.assertEqual(self.speech.sqrt(r"\sqrt{16}", 5), (" root 16", 9))

    def test_simple_root_single_letter_alphabetic_var_as_a_number(self):
        """Check translation of square root where number is an alphabetic variable consisting of a single letter."""
        self.assertEqual(self.speech.sqrt(r"\sqrt{x}", 5), (" root x", 8))

    def test_simple_root_multi_letter_alphabetic_var_as_a_number(self):
        """Check translation of square root where number is an alphabetic variable composed of multiple letters."""
        self.assertEqual(
            self.speech.sqrt(r"\sqrt{xy}", 5),
            (" begin  root xy end root", 9)
        )

    def test_square_root_degree_specified_explicitly(self):
        """Test for square root where degree is specified by the user."""
        self.assertEqual(
            self.speech.sqrt(r"\sqrt[2]{16}", 5),
            ("square root 16", 12)
        )

    def test_cube_root(self):
        """Test translation of cube root."""
        self.assertEqual(
            self.speech.sqrt(r"\sqrt[3]{27}", 5),
            ("cube root 27", 12)
        )

    def test_root_empty_degree(self):
        """Test translation of root where degree was kept empty, but provided."""
        self.assertEqual(
            self.speech.sqrt(r"\sqrt[]{27}", 5),  # Is this even valid LaTeX?
            (" root 27", 11)
        )

    def test_root_with_a_numeric_degree(self):
        """Test translation of a root with a numeric degre."""
        self.assertEqual(
            self.speech.sqrt(r"\sqrt[4]{16}", 5),
            ("4th root 16", 12)
        )
