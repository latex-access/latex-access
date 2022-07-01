import unittest
from latex_access import speech

class TestSpeech(unittest.TestCase):

    def setUp(self):
        """Creates a speech instance."""
        self.speech = speech.speech()

    def test_super(self):
        """Tests translation of superscripts."""
        self.assertEqual(self.speech.super('2', 0), (' squared ', 1))
        self.assertEqual(self.speech.super('3', 0), (' cubed ', 1))
        self.assertEqual(self.speech.super('{\prime}', 0), (' prime ', 8))
        self.assertEqual(self.speech.super('{10}', 0), (' to the <sup> 10 </sup> ', 4))

    def test_sqrt(self):
        """Tests translations of roots."""
        self.assertEqual(self.speech.sqrt('2', 0), (' root 2', 1))
        self.assertEqual(self.speech.sqrt('{16}', 0), (' root 16', 4))
        self.assertEqual(self.speech.sqrt('[3]{27}', 0), ('cube root 27', 7))
        self.assertEqual(self.speech.sqrt('[5]{32}', 0), ('5th root 32', 7))
        #self.assertEqual(self.speech.sqrt('{2^2}', 0), (' begin  root 2 squared  end root', 5))

    def test_frac(self):
        """Tests translactions of fractions."""
        self.assertEqual(self.speech.frac('{1}{2}', 0), ('1 half ', 6))
        self.assertEqual(self.speech.frac('{3}{4}', 0), ('3 quarters ', 6))
        self.assertEqual(self.speech.frac('{x}{y}', 0), (' x over y ', 6))
        self.assertEqual(self.speech.frac('{5}{11}', 0), (' begin frac 5 over 11 end frac ', 7))

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
        self.assertEqual(self.speech.log('_{2}8', 0), (' log base 2 of ', 4))
