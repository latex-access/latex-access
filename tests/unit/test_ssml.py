import os.path
import unittest

from latex_access.speech_translators.experimental import ssml


class TestSsml(unittest.TestCase):
    def setUp(self):
        """Creates a speech instance."""
        self.ssml = ssml.speech()

    def test_prosody(self):
        """Tests that prosody is added to table."""
        self.assertEqual(self.ssml.table['_'], ('<prosody pitch="-15%">','</prosody>'))

    def test_sqrt(self):
        """Tests translations of roots."""
        self.assertEqual(self.ssml.sqrt('2', 0), (' root 2', 1))
        self.assertEqual(self.ssml.sqrt('{2x}', 0), (' begin root <break strength="strong">2x end root <break strength="strong">', 4))

    def test_symbols_from_correct_table_loaded(self):
        """Translator for SSML should load symbols from the base translator."""
        self.assertEqual(self.ssml.table["="], "equals")

    def test_table_located_from_the_base_package(self):
        """Translator for SSML should look for table files in the base directory."""
        tables_dir = self.ssml.get_buildin_tables_dir()
        self.assertTrue(os.path.isdir(tables_dir))
        self.assertIn("speech.table", os.listdir(tables_dir))
