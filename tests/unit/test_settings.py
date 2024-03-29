import unittest
import os
import tempfile
from latex_access import settings as settings_module


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

    def __init__(self, remove_dollars=False, capitalisation="6dot"):
        super(FakeTranslator, self).__init__()
        self.remove_dollars = remove_dollars
        self.capitalisation = capitalisation
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

    def test_no_change_to_settings_when_empty_file_provided(self):
        """Test that empty config file causes no change in settings."""
        sample_config_path = os.path.join(
            HERE,
            'test_settings_empty_file.txt'
        )
        settings_loaded = settings_module.loadSettings(sample_config_path)
        self.assertTrue(settings_loaded)
        self.assertEqual(
            settings_module.settings,
            self.settings_copy
        )

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

    def test_nemeth_translator_used_when_configured(self):
        """Tests that translator for Nemeth is used when configured.

        Previously we used to check if the returned object
        is of a particular type, but from the end user perspective
        it is more interesting,
        if the translations matches their expectations
        i.e. correct Braille notation is used.
        """
        settings_module.settings['brailletable'] = 'nemeth'
        translator = settings_module.brailleTableToUse()
        self.assertEqual(translator.translate(r"\frac{1}{2}"), "1/2")
        # Let's also check if correct table file was loaded.
        self.assertEqual(translator.translate(r"\Chi"), ".,&")

    def test_ueb_translator_used_when_configured(self):
        """Tests that translator for UEB is used when configured.

        For more comments on the used approach
        see docstring of `test_nemeth_translator_used_when_configured`.
        """
        settings_module.settings['brailletable'] = 'ueb'
        translator = settings_module.brailleTableToUse()
        self.assertEqual(translator.translate(r"\frac{1}{2}"), "#a/b")
        # Let's also check if correct table file was loaded.
        self.assertEqual(translator.translate(r"\Chi"), ",.c")

    def test_default_braille_translator_used_as_a_fallback(self):
        """Translator for Nemeth should be used when configured one does not exist.

        This checks for a corner case, where the name of a Braille translator
        specified in a config file is invalid
        (there is no such module in the ``braille_translators`` package).
        The translator used as a fallback is the one for Nemeth
        which was always the default one.
        """
        settings_module.settings['brailletable'] = 'foobar'
        translator = settings_module.brailleTableToUse()
        self.assertEqual(translator.translate(r"\frac{1}{2}"), "1/2")
        # Let's also check if correct table file was loaded.
        self.assertEqual(translator.translate(r"\Chi"), ".,&")

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
        fakeSpeechTranslator = FakeTranslator(remove_dollars=True)
        settings_module.settings["speakdollars"] = "true"
        settingsLoaded = settings_module.activateSettings(
            {"speak": fakeSpeechTranslator}
        )
        self.assertTrue(settingsLoaded)
        # Not sure why the value from settings is negated!
        self.assertFalse(fakeSpeechTranslator.remove_dollars )

    def test_dollar_presentation_modified_in_braille(self):
        fakeBrailleTranslator = FakeTranslator(remove_dollars=True)
        settings_module.settings["brailledollars"] = "true"
        settingsLoaded = settings_module.activateSettings(
            {"braille": fakeBrailleTranslator}
        )
        self.assertTrue(settingsLoaded)
        # Not sure why the value from settings is negated!
        self.assertFalse(fakeBrailleTranslator.remove_dollars )

    def test_capitalization_from_settings_set_onbraillle_translator(self):
        fakeBrailleTranslator = FakeTranslator()  # Create with default params
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
        fakeBrailleTranslator = FakeTranslator()
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
        fakeSpeechTranslator = FakeTranslator()
        with tempfile.NamedTemporaryFile() as speech_fake_table:
            settings_module.settings["speechfile"] = speech_fake_table.name
            settingsLoaded = settings_module.activateSettings(
                {"speak": fakeSpeechTranslator}
            )
            self.assertTrue(settingsLoaded)
            self.assertEqual(fakeSpeechTranslator.loaded_files, [speech_fake_table.name])

    def test_speech_translator_imported_by_name(self):
        """Non-experimental speech translator can be imported by name."""
        translator = settings_module.get_speech_translator("hungarian_speech")()
        self.assertEqual(translator.table[r"\ddot"], ("", "duplapont"))

    def test_experimental_speech_translator_imported_by_name_experimental_requested(self):
        """When experimental modules are explicitly requested,
        experimental speech translator can be imported by name.
        """
        translator = settings_module.get_speech_translator("ssml", experimental=True)()
        self.assertEqual(translator.table["_"], ('<prosody pitch="-15%">','</prosody>'))

    def test_experimental_speech_translator_imported_by_name(self):
        """Even though experimental module is not requested,
        it can be imported by an smart user, by appropriately prepared name.
        """
        translator = settings_module.get_speech_translator("experimental.ssml", experimental=False)()
        self.assertEqual(translator.table["_"], ('<prosody pitch="-15%">','</prosody>'))
