# -*- coding: utf8 -*-
import unittest
from parameterized import parameterized
from latex_access import polish_speech


class TestPolishSpeech(unittest.TestCase):
    def setUp(self):
        """Creates a speech instance."""
        self.speech = polish_speech.speech()

    def test_adding_file(self):
        """Tests adding of speechtable file."""
        self.assertEqual(self.speech.files, ['polish_speech.table'])

    def test_adding_to_table(self):
        """Tests adding of new elements to speech table."""
        self.assertEqual(self.speech.table['^'], self.speech.super)
        self.assertEqual(self.speech.table['\\mathcal'], ('<mathcal>', '</mathcal>'))
        self.assertEqual(self.speech.table['\\abs'], (u'wartość bezwzględna ', u' koniec wartości'))

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
        self.assertEqual(self.speech.super('2', 0), (' kwadrat ', 1))
        self.assertEqual(self.speech.super('3', 0), (u' sześcian ', 1))
        self.assertEqual(self.speech.super(r'{\circ}', 0), (u'stopni', 7))
        self.assertEqual(self.speech.super(r'{\prime}', 0), (' prim ', 8))
        self.assertEqual(self.speech.super('{10}', 0), (u' do potęgi 10 ', 4))
        self.assertEqual(self.speech.super('{-4}', 0), (u' do potęgi  minus 4 ', 4))

    def test_sub(self):
        """Tests translations of subscripts."""
        self.assertEqual(self.speech.sub('1', 0), (u' indeks dolny 1', 1))
        self.assertEqual(self.speech.sub('{x+1}', 0), (u' indeks dolny x dodać 1 koniec indeksu ', 5))

    def test_sqrt(self):
        """Tests translations of roots."""
        self.assertEqual(self.speech.sqrt('[2]{9}', 0), (u' pierwiastek z 9 ', 6))
        self.assertEqual(self.speech.sqrt('[3]{27}', 0), (u' pierwiastek trzeciego stopnia z 27 ', 7))
        self.assertEqual(self.speech.sqrt('[N]{27}', 0), (u' pierwiastek stopnia en z 27 ', 7))
        self.assertEqual(self.speech.sqrt('[X]{27}', 0), (u' pierwiastek stopnia x z 27 ', 7))
        self.assertEqual(self.speech.sqrt('[]{27}', 0), (u'27 ', 6))
        self.assertEqual(self.speech.sqrt('[4]{27}', 0), (u' pierwiastek stopnia 4 z 27 ', 7))
        self.assertEqual(self.speech.sqrt('[2]{2x}', 0), (u' pierwiastek z  początek pierwiastka 2x koniec pierwiastka ', 7))
        self.assertEqual(self.speech.sqrt('{4}', 0), (u' pierwiastek z 4 ', 3))
        self.assertEqual(self.speech.sqrt('{2x}', 0), (u' początek pierwiastka 2x koniec pierwiastka ', 4))

    def test_frac(self):
        """Tests translations of fractions."""
        self.assertEqual(self.speech.frac('{1}{2}', 0), ('1 przez 2 ', 6))
        self.assertEqual(self.speech.frac('{1}', 0), (u' początek ułamka 1 przez  koniec ułamka ', 3))
        self.assertEqual(self.speech.frac('{3}{4}', 0), ('3 przez 4 ', 6))
        self.assertEqual(self.speech.frac('{x}{y}', 0), (u' x  przez y ', 6))
        self.assertEqual(self.speech.frac('{5}{11}', 0), (u' początek ułamka 5 przez 11 koniec ułamka ', 7))
        self.assertEqual(self.speech.frac('1\\frac{1}{2}', 6), ('i 1 przez 2 ', 12))

    def test_dsfrac(self):
        """Unclear what `\\dsfrac`` does"""
        self.assertEqual(self.speech.dsfrac("{1}", 0), (u" początek ułamka 1 koniec ułamka ", 3))

    @parameterized.expand([(polish_speech.speech.integral, u"całka"), (polish_speech.speech.dbintegral, u"całka podwójna"),
    (polish_speech.speech.ddintegral, u"całka potrójna"), (polish_speech.speech.ointegral, "loop integral")])
    def test_integrals(self, function, result):
        """Tests translations of integrals."""
        self.assertEqual(function(self.speech, '_{2}^{4}', 0), (u' %s  od 2 do 4 z ' % (result), 8))
        self.assertEqual(function(self.speech, '_{2}', 0), (' %s <sub>2</sub>' % (result), 4))
        self.assertEqual(function(self.speech, '', 0), (' %s ' % (result), 0))

    @parameterized.expand([(polish_speech.speech.sum, "suma"), (polish_speech.speech.prod, "iloczyn"),
                           (polish_speech.speech.union, "suma"), (polish_speech.speech.cap, "iloczyn")])
    def test_other_functions(self, function, result):
        """Tests translations of sums, products, unions and intersections."""
        self.assertEqual(function(self.speech, '_{2}^{4}', 0), (u' %s  od 2 do 4 z ' % (result), 8))
        self.assertEqual(function(self.speech, '_{2}', 0), (' %s <sub>2</sub>' % (result), 4))
        self.assertEqual(function(self.speech, '', 0), (' %s ' % (result), 0))

    def test_tag(self):
        """Tests translations of tags."""
        self.assertEqual(self.speech.tag('{10}', 0), (' tag lewy nawias 10 prawy nawias ', 4))

    def test_log(self):
        """Tests translations of logarithms."""
        self.assertEqual(self.speech.log(' 5', 0), (' logarytm z ', 1))
        self.assertEqual(self.speech.log('_{2}8', 0), (' logarytm o podstawie 2 z ', 4))

    def test_vect(self):
        """Tests translations of vectors."""
        self.assertEqual(self.speech.vect('x', 0), ('x wektor ', 1))
        self.assertEqual(self.speech.vect('{xy}', 0 ), (u' wektor x y koniec wektora ', 4))

    def test_binom(self):
        """Tests translations of binomials."""
        self.assertEqual(self.speech.binom('{n}{k}', 0), (' n brane po k ', 6))
        self.assertEqual(self.speech.binom('{n}', 0), (u' początek dwumianu n brane po  koniec dwumianu ', 3))
        self.assertEqual(self.speech.binom('{n}{k+1}', 0), (u' początek dwumianu n brane po k dodać 1 koniec dwumianu ', 8))

    def test_ln(self):
        """Tests translations of natural logarithms."""
        self.assertEqual(self.speech.ln(' x', 0), (' logarytm naturalny ', 1))
        self.assertEqual(self.speech.ln('_{e}x', 0), (u' logarytm naturalny o podstawie e z ', 4))

    def test_angle_no_coordinates(self):
        """Check translation for angles without degrees, minutes and seconds."""
        self.assertEqual(self.speech.ang(r"\ang{30}", 4), ("30 stopni", 8))

    def test_angle_minutes_present(self):
        """Check translation for angles where minutes are present.

        Note that behaviour of ang method in this case seems incorrect
        the trailing semicolon after minutes
        causes unnecesary seconds to be added.
        """
        self.assertEqual(
            self.speech.ang(r"\ang{52;58.110;}", 4),
            ("52 stopni 58.110 minut  sekund", 16)
        )

    def test_angle_minutes_present_no_trailing_semicolon(self):
        """Check translation for angles where minutes are present."""
        self.assertEqual(
            self.speech.ang(r"\ang{52;58.110}", 4),
            ("52 stopni 58.110 minut", 15)
        )

    def test_angle_minutes_and_seconds_present(self):
        """Check translation for angles with both minutes and seconds."""
        self.assertEqual(
            self.speech.ang(r"\ang{52;58;13}", 4),
            ("52 stopni 58 minut 13 sekund", 14)
        )

    def test_angle_minutes_and_seconds_present_trailing_semicolon(self):
        """Check translation for angles with both minutes and seconds + semicolon at the end."""
        self.assertEqual(
            self.speech.ang(r"\ang{52;58;13;}", 4),
            ("52 stopni 58 minut 13 sekund ", 15)
        )

    def test_angle_semicolon_after_seconds_kept_verbatim(self):
        """Check translation for angles with minutes, seconds and additional semicolon at the end.

        Note that it is unclear how exactly such coordinate looks when compiled.
        It is also unclear if keeping the part after semicolon
        which ends seconds is the best translation possible.
        """
        self.assertEqual(
            self.speech.ang(r"\ang{52;58;13;22;}", 4),
            ("52 stopni 58 minut 13 sekund 22;", 18)
        )
