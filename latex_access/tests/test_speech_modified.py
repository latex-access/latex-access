import unittest
from parameterized import parameterized
from .speech_modifed_wrapper import speech_modified
try:
    speech_modified.speech()
except AttributeError:
    failed_to_import = True
else:
    failed_to_import = False


@unittest.skipIf(failed_to_import, "Speech modified module is incompatible with current version of python.")
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
        self.assertEqual(self.speech.super('{\prime}', 0), (' prime ', 8))
        self.assertEqual(self.speech.super('{10}', 0), (' to the <sup> 10 </sup> ', 4))
        self.assertEqual(self.speech.super('{-4}', 0), (u' to the <sup>  minus 4 </sup> ', 4))

#    def test_sqrt(self):
#        """Tests translations of roots."""
#        self.assertEqual

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

    def test_ang(self):
        """Tests translations of angles."""
        self.assertEqual(self.speech.ang('{30}', 0), ('30 degrees', 4))
        self.assertEqual(self.speech.ang('{45;10}', 0), ('45 degrees 10 minutes', 7))
        self.assertEqual(self.speech.ang('{60;15;20}', 0), ('60 degrees 15 minutes 20 seconds', 10))
        self.assertEqual(self.speech.ang('{60;15;20;25}', 0), ('60 degrees 15 minutes 20 seconds 25', 13))

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
