"""Contains translators for all supported Braille notations.

Each translator should be a module placed in this package,
Named according to the notation it supports.
Since these names are used in the configuration file,
being user friendly is a plus.
The module should define a class named `BrailleTranslator`,
which inherits from `latex_access.translator`.
Note that translator instances should not usually be constructed directly by the user.
"""
