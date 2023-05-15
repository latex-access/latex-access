"""Contains all supported speech translators.

Each translator should be a module placed in this package,
Preferably named so that it is clear which language it supports.
Since these names are used in the configuration file,
being user friendly is a plus.
The module should define a class named `speech`,
which inherits from `latex_access.translator`.
Note that translator instances should not usually be constructed directly by the user.
"""
