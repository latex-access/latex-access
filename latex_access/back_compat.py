"""Holds various constants used to preserve backwards compatibility with Python 2."""

try:
    import __builtin__
except ImportError:
    STR_AND_UNICODE = str
    STR_TYPE = str  # No separate type for non-unicode strings in Python 3.
else:  # Python 2
    STR_AND_UNICODE = __builtin__.basestring
    STR_TYPE = __builtin__.str
