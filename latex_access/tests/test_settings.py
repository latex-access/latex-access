import unittest
import os
import latex_access
from latex_access import settings as settings_module
from latex_access import  nemeth
from latex_access import ueb

try:
    translator = ueb.ueb()
    incompatible = False
except AttributeError:
    incompatible = True

class TestSettings(unittest.TestCase):
    def setUp(self):
        self.settings_copy = dict(settings_module.settings)

    def tearDown(self):
        settings_module.settings = self.settings_copy

    def test_load_settings(self):
        """Tests that settings are loaded from an existing file."""
        tests_directory = os.path.abspath(os.path.dirname(__file__))
        sample_config_path = os.path.join(tests_directory, 'test_settings_correct.txt')
        self.assertTrue(settings_module.loadSettings(sample_config_path))
        self.assertFalse(settings_module.loadSettings(os.path.join(tests_directory, 'foo.txt')))

    def test_read_settings(self):
        """Tests that settings are read from a file."""
        settings_module.settings = {}
        tests_directory = os.path.abspath(os.path.dirname(__file__))
        sample_config_path = os.path.join(tests_directory, 'test_settings_correct.txt')
        settings_module.loadSettings(sample_config_path)
        self.assertEqual(settings_module.settings, {'brailledollars': 'true', 'speakdollars': 'true', 'capitalisation': '6dot'})

    def test_incorrect_input(self):
        """Tests that a file with incorrect data doesn't modify settings."""
        tests_directory = os.path.abspath(os.path.dirname(__file__))
        sample_config_path = os.path.join(tests_directory, 'test_settings_incorrect.txt')
        settings_module.loadSettings(sample_config_path)
        self.assertEqual(settings_module.settings, self.settings_copy)

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

    def test_nemeth_is_used(self):
        """Tests that an instance of nemeth is returned based on setting's                 value."""
        settings_module.settings['brailletable'] = 'nemeth'
        self.assertIsInstance(settings_module.brailleTableToUse(), nemeth.nemeth)

    @unittest.skipIf(incompatible, 'This test fails in Python 3 because of iteritems being used.')
    def test_ueb_is_used(self):
        """Tests that an instance of ueb is returned based on setting's                 value."""
        settings_module.settings['brailletable'] = 'ueb'
        self.assertIsInstance(settings_module.brailleTableToUse(), ueb.ueb)
