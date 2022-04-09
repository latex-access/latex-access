import unittest
import latex_access
from latex_access import settings as settings_module

try:
    settings_module.settings['brailletable'] = 'nemeth'
    settings_module.brailleTableToUse()
    settings_module.settings['brailletable'] = 'ueb'
    settings_module.brailleTableToUse()
    incompatible = False
except AttributeError:
    incompatible = True

class TestSettings(unittest.TestCase):
    def test_load_settings(self):
        """Tests that settings are read from a file."""
        settings_module.settings = {}
        settings_module.loadSettings('test_settings.txt')
        self.assertEqual(settings_module.settings, {'brailledollars': 'true', 'speakdollars': 'true', 'capitalisation':'6dot'})

    def test_booleanise_setting(self):
        """Tests that setting's value is converted to boolean."""
        settings_module.settings['brailledollars'] = 'True'
        settings_module.settings['foo'] = 'bar'
        self.assertTrue(settings_module.booleaniseSetting('brailledollars'))
        self.assertFalse(settings_module.booleaniseSetting('foo'))

    def test_get_setting(self):
        """Tests that value is returned for given setting."""
        settings_module.settings['speakdollars'] = True
        self.assertTrue(settings_module.getSetting('speakdollars'))
        self.assertFalse(settings_module.getSetting('foo'))

    @unittest.skipIf(incompatible, 'This test fails in Python 3 because of iteritems being used.')
    def test_braille_table_to_use(self):
        """Tests that proper instance of braille table is returned based on setting's                 value."""
        settings_module.settings['brailletable'] = 'nemeth'
        self.assertIsInstance(settings_module.brailleTableToUse(), latex_access.nemeth.nemeth)
        settings_module.settings['brailletable'] = 'ueb'
        self.assertIsInstance(settings_module.brailleTableToUse(), latex_access.ueb.ueb)
