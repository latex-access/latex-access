import unittest
import latex_access
from latex_access.settings import *

try:
    settings['brailletable'] = 'nemeth'
    brailleTableToUse()
    settings['brailletable'] = 'ueb'
    brailleTableToUse()
    incompatible = False
except AttributeError:
    incompatible = True

class TestSettings(unittest.TestCase):
    def test_booleanise_setting(self):
        """Tests that setting's value is converted to boolean."""
        settings['brailledollars'] = 'True'
        settings['foo'] = 'bar'
        self.assertTrue(booleaniseSetting('brailledollars'))
        self.assertFalse(booleaniseSetting('foo'))

    def test_get_setting(self):
        """Tests that value is returned for given setting."""
        settings['speakdollars'] = True
        self.assertTrue(getSetting('speakdollars'))
        self.assertFalse(getSetting('foo'))

    @unittest.skipIf(incompatible, 'This test fails in Python 3 because of iteritems being used.')
    def test_braille_table_to_use(self):
        """Tests that proper instance of braille table is returned based on setting's                 value."""
        settings['brailletable'] = 'nemeth'
        self.assertIsInstance(brailleTableToUse(), latex_access.nemeth.nemeth)
        settings['brailletable'] = 'ueb'
        self.assertIsInstance(brailleTableToUse(), latex_access.ueb.ueb)
