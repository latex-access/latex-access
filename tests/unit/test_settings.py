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

    def __init__(self):
        super(_FakePreprocessor, self).__init__()
        self.loadedTables = list()

    def read(self, path):
        self.loadedTables.append(path)


class FakeTranslator(object):
    """Minimal interface necessary to mockup a braille / speech translator."""

    def __init__(self, attrsToSet):
        for attrName, attrInitialVal in attrsToSet.items():
            setattr(self, attrName, attrInitialVal)
        self.loaded_files = list()

    def load_file(self, pathToLoad):
        self.loaded_files.append(pathToLoad)


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

    def test_settings_file_lines_ignored(self):
        """Test that lines which consist only of white spaces
        are ignored when reading settings file."""
        settings_module.settings = {}
        sample_config_path = os.path.join(
            HERE,
            'test_settings_white_spaces_only_lines.txt'
        )
        settings_module.loadSettings(sample_config_path)
        self.assertEqual(
            settings_module.settings,
            {
                'brailledollars': 'true',
                'speakdollars': 'true',
                'capitalisation': '6dot'
            }
        )

    def test_booleanise_setting(self):
        """Tests that setting's value is converted to boolean."""
        settings_module.settings['brailledollars'] = 'True'
        settings_module.settings['foo'] = 'bar'
        self.assertTrue(settings_module.booleaniseSetting('brailledollars'))
        self.assertFalse(settings_module.booleaniseSetting('foo'))

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

    def test_dollar_announcement_modified_in_speech(self):
        fakeSpeechTranslator = FakeTranslator({"remove_dollars": None})
        settings_module.settings["speakdollars"] = True
        settingsLoaded = settings_module.activateSettings(
            {"speak": fakeSpeechTranslator}
        )
        self.assertTrue(settingsLoaded)
        # Not sure why the value from settings is negated!
        self.assertFalse(fakeSpeechTranslator.remove_dollars )

    def test_dollar_presentation_modified_in_braille(self):
        fakeBrailleTranslator = FakeTranslator({"remove_dollars": None})
        settings_module.settings["brailledollars"] = True
        settingsLoaded = settings_module.activateSettings(
            {"braille": fakeBrailleTranslator}
        )
        self.assertTrue(settingsLoaded)
        # Not sure why the value from settings is negated!
        self.assertFalse(fakeBrailleTranslator.remove_dollars )

    def test_capitalization_from_settings_set_onbraillle_translator(self):
        fakeBrailleTranslator = FakeTranslator({"capitalisation": ""})
        settings_module.settings["capitalisation"] = "8dot"
        settingsLoaded = settings_module.activateSettings(
            {"braille": fakeBrailleTranslator}
        )
        self.assertTrue(settingsLoaded)
        self.assertEqual(fakeBrailleTranslator.capitalisation, "8dot")

    def test_preprocessor_no_change_when_loading_from_non_existent_file(self):
        """Ensures that when the file
        containing preprocessor entries does not exist
        no entries are loaded,
        and there is no exception raised.
        """
        #Even though `tempfile.mktemp` is deprecatedd it does exactly what
        # we need i.e. returns name of a temporary file which does not exist
        # at the time of the call.
        preprocFileName = tempfile.mktemp()
        fakePreprocessor = _FakePreprocessor()
        settings_module.settings["preprocessorfile"] = preprocFileName
        settingsLoaded = settings_module.activateSettings(
            {"preprocessor": fakePreprocessor}
        )
        self.assertTrue(settingsLoaded)
        self.assertEqual(fakePreprocessor.loadedTables, [])

    def test_table_for_ueb_loaded(self):
        """Ensure that when UEB is used the correct file is loaded."""
        fakeBrailleTranslator = FakeTranslator({})
        settings_module.settings["brailletable"] = "ueb"
        with tempfile.NamedTemporaryFile() as ueb_fake_table:
            settings_module.settings["uebfile"] = ueb_fake_table.name
            settingsLoaded = settings_module.activateSettings(
                {"braille": fakeBrailleTranslator}
            )
            self.assertTrue(settingsLoaded)
            self.assertEqual(fakeBrailleTranslator.loaded_files, [ueb_fake_table.name])

    def test_table_for_speech_loaded(self):
        """Ensure that when speech table exists it is loaded."""
        fakeSpeechTranslator = FakeTranslator({})
        with tempfile.NamedTemporaryFile() as speech_fake_table:
            settings_module.settings["speechfile"] = speech_fake_table.name
            settingsLoaded = settings_module.activateSettings(
                {"speak": fakeSpeechTranslator}
            )
            self.assertTrue(settingsLoaded)
            self.assertEqual(fakeSpeechTranslator.loaded_files, [speech_fake_table.name])
