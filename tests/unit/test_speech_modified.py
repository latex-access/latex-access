import unittest

from parameterized import parameterized

from latex_access.speech_translators import speech_modified


class TestSpeechModified(unittest.TestCase):
    def setUp(self):
        """Creates a speech instance."""
        self.speech = speech_modified.speech()

    def test_adding_file(self):
        """Tests adding of speechtable file."""
        self.assertEqual(self.speech.files, ['speech.table'])

    def test_adding_to_table(self):
        """Tests adding of new elements to speech table."""
        self.assertEqual(self.speech.table['\\ln'], self.speech.ln)
        self.assertEqual(self.speech.table['_'], ("<sub>","</sub>"))
        self.assertEqual(self.speech.table['\\cup'], self.speech.union)

    def test_space(self):
        """Tests creation of space attribute."""
        self.assertEqual(self.speech.space, ' ')
    def test_correct(self):
        """Tests replacing groups of letters."""
        self.assertEqual(self.speech.correct('xy'), 'x y')
        self.assertEqual(self.speech.correct('yy'), 'y y')
        self.assertEqual(self.speech.correct('isx'), 'i es x')

    def test_super(self):
        """Tests translations of superscripts."""
        self.assertEqual(self.speech.super('2', 0), (' squared ', 1))
        self.assertEqual(self.speech.super('3', 0), (' cubed ', 1))
        self.assertEqual(self.speech.super(r'{\prime}', 0), (' prime ', 8))
        self.assertEqual(self.speech.super('{10}', 0), (' to the <sup> 10 </sup> ', 4))
        self.assertEqual(self.speech.super('{-4}', 0), (u' to the <sup>  minus 4 </sup> ', 4))

    def test_square_roots(self):
        """Tests translations of square roots."""
        self.assertEqual(self.speech.sqrt("{2}", 0), (u" root 2 ", 3))
        self.assertEqual(self.speech.sqrt("{x}", 0), (u" root x ", 3))
        self.assertEqual(self.speech.sqrt("{\\frac{1}{2}}", 0), (u" root of 1 half  end root ", 13))

    def test_roots_degree_specified_explicitly(self):
        """Test for roots where degree is specified by the user."""
        self.assertEqual(
            self.speech.sqrt("\\sqrt[2]{16}", 5),
            (u" square root of 16 end root ", 12)
        )
        self.assertEqual(
            self.speech.sqrt("\\sqrt[3]{27}", 5),
            (u" cube root of 27 end root ", 12)
        )
        self.assertEqual(
            self.speech.sqrt("\\sqrt[N]{27}", 5),
            (u"enth root of 27 end root ", 12)
        )
        self.assertEqual(
            self.speech.sqrt("\\sqrt[x]{27}", 5),
            (u"exth root of 27 end root ", 12)
        )
        self.assertEqual(
            self.speech.sqrt("\\sqrt[]{27}", 5),  # Is this even valid LaTeX?
            (u"root of 27 end root ", 11)
        )
        self.assertEqual(
            self.speech.sqrt("\\sqrt[4]{16}", 5),
            (u"4th root of 16 end root ", 12)
        )
        self.assertEqual(
            self.speech.sqrt("\\sqrt[3]{8}", 5),
            (u" cube root 8", 11)
        )

    def test_dsfrac(self):
        """Unclear what `\\dsfrac`` does"""
        self.assertEqual(self.speech.dsfrac("{1}", 0), (u" begin frac 1 end frac ", 3))

    def test_frac(self):
        """Tests translactions of fractions."""
        self.assertEqual(self.speech.frac('{1}{2}', 0), ('1 half ', 6))
        self.assertEqual(self.speech.frac('{1}', 0), (' begin frac, 1 over , end frac ', 3))
        self.assertEqual(self.speech.frac('{3}{4}', 0), ('3 quarters ', 6))
        self.assertEqual(self.speech.frac('{x}{y}', 0), (' x over y ', 6))
        self.assertEqual(self.speech.frac('{5}{11}', 0), (' begin frac, 5 over 11, end frac ', 7))

    @parameterized.expand([(speech_modified.speech.integral, "integral"), (speech_modified.speech.dbintegral, "double integral"),
    (speech_modified.speech.ddintegral, "triple integral"), (speech_modified.speech.ointegral, "loop integral")])
    def test_integrals(self, function, result):
        """Tests translations of integrals."""
        self.assertEqual(function(self.speech, '_{2}^{4}', 0), (u' %s  from 2 to 4 of ' % (result), 8))
        self.assertEqual(function(self.speech, '_{2}', 0), (' %s <sub>2</sub>' % (result), 4))
        self.assertEqual(function(self.speech, '', 0), (' %s ' % (result), 0))

    @parameterized.expand([(speech_modified.speech.sum, "sum"), (speech_modified.speech.prod, "product"),
    (speech_modified.speech.union, "union"), (speech_modified.speech.cap, "intersection")])
    def test_other_functions(self, function, result):
        """Tests translatios of sums, products, unions and intersections."""
        self.assertEqual(function(self.speech, '_{2}^{4}', 0), (u' %s  where 2 to 4 of ' % (result), 8))
        self.assertEqual(function(self.speech, '_{2}', 0), (' %s <sub>2</sub>' % (result), 4))
        self.assertEqual(function(self.speech, '', 0), (' %s ' % (result), 0))

    def test_tag(self):
        """Tests translations of tags."""
        self.assertEqual(self.speech.tag('{10}', 0), (' tag left paren 10 right paren ', 4))

    def test_log(self):
        """Tests translations of logarithms."""
        self.assertEqual(self.speech.log(' 5', 0), (' log ', 1))
        self.assertEqual(self.speech.log('_{2}8', 0), (' log base 2 of ', 4))

    def test_vect(self):
        """Tests translations of vectors."""
        self.assertEqual(self.speech.vect('x', 0), ('x vector ', 1))
        self.assertEqual(self.speech.vect('{xy}', 0 ), (u' vector x y end vector ', 4))

    def test_binom(self):
        """Tests translations of binomials."""
        self.assertEqual(self.speech.binom('{n}{k}', 0), (' n choose k ', 6))
        self.assertEqual(self.speech.binom('{n}', 0), (u' begin binomial n choose  end binomial ', 3))
        self.assertEqual(self.speech.binom('{n}{k+1}', 0), (u' begin binomial n choose k plus 1 end binomial ', 8))

    def test_ln(self):
        """Tests translations of natural logarithms."""
        self.assertEqual(self.speech.ln(' x', 0), (' loen ', 1))
        self.assertEqual(self.speech.ln('_{e}x', 0), (' loen base e of ', 4))

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
