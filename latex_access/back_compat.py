"""Holds various constants used to preserve backwards compatibility with Python 2."""

try:
    import __builtin__
except ImportError:
    STR_AND_UNICODE = str
else:  # Python 2
    STR_AND_UNICODE = __builtin__.basestring
