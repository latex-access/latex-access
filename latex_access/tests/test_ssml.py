import unittest
try:
    from latex_access import ssml
except ImportError:
    failedToImport = True
else:
    failedToImport = False

@unittest.skipIf(failedToImport, "Ssml module is not compatible with current version of Python.")
class TestSsml(unittest.TestCase):
    def setUp(self):
        """Creates a speech instance."""
        self.ssml = ssml.ssml()

    def test_prosody(self):
        """Tests that prosody is added to table."""
        self.assertEqual(self.ssml.table['_'], ('<prosody pitch="-15%">','</prosody>'))

    def test_sqrt(self):
        """Tests translations of roots."""
        self.assertEqual(self.ssml.sqrt('2', 0), (' root 2', 1))
        self.assertEqual(self.ssml.sqrt('{2x}', 0), (' begin root <break strength="strong">2x end root <break strength="strong">', 4))