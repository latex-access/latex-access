import unittest
import os
import tempfile
from latex_access import settings as settings_module
from latex_access import nemeth
from latex_access import ueb

try:
    ueb.ueb()
    incompatible = False
except AttributeError:
    incompatible = True

HERE = os.path.abspath(os.path.dirname(__file__))


class _FakePreprocessor(object):
    """Implements minimal interface
    necessary to mockup a preprocessor instance.
    """

    loadedTables = list()

    def read(self, path):
        self.loadedTables.append(path)


class TestSettings(unittest.TestCase):
    def setUp(self):
        self.settings_copy = dict(settings_module.settings)

    def tearDown(self):
        settings_module.settings = self.settings_copy

    def test_load_settings(self):
        """Tests that settings are loaded from an existing file."""
        sample_config_path = os.path.join(HERE, 'test_settings_correct.txt')
        self.assertTrue(settings_module.loadSettings(sample_config_path))
        self.assertFalse(settings_module.loadSettings(os.path.join(HERE, 'foo.txt')))

    def test_read_settings(self):
        """Tests that settings are read from a file."""
        settings_module.settings = {}
        sample_config_path = os.path.join(HERE, 'test_settings_correct.txt')
        settings_module.loadSettings(sample_config_path)
        self.assertEqual(settings_module.settings, {'brailledollars': 'true', 'speakdollars': 'true', 'capitalisation': '6dot'})

    def test_incorrect_input(self):
        """Tests that a file with incorrect data doesn't modify settings."""
        sample_config_path = os.path.join(HERE, 'test_settings_incorrect.txt')
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

    def test_preprocessor_entries_loaded_after_activation(self):
        """Test that entries for the preprocessor are loaded from a file."""
        fakePreprocessor = _FakePreprocessor()
        with tempfile.NamedTemporaryFile() as preprocFakeFile:
            settings_module.settings["preprocessorfile"] = preprocFakeFile.name
            settingsLoaded = settings_module.activateSettings(
                {"preprocessor": fakePreprocessor}
            )
            self.assertTrue(settingsLoaded)
            self.assertEqual(
                fakePreprocessor.loadedTables,
                [preprocFakeFile.name]
            )
