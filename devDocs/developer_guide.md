# Latex-access developer documentation

## Introduction
Latex-access is a Python library containing various utilities used to make LaTeX documents easier to read for users of screen readers / Braille displays.
It interfaces with screen readers for various platforms.
If you want to use it you should consult the documentation for the interface to a particular screen reader you are interested in, and not this manual.
This guide is for you if:

* You are interested in the inner workings of the library just to satisfy your curiosity
* You have found a bug, and want to understand the code structure to either create a more comprehensive bug report, or fix it yourself
* You want to create an interface to a screen reader / editor which is not yet supported
* You want to create a translator for language / Braille notation which is not supported yet.


## General code structure
All Latex-access's modules are placed in the single Python package called `latex_access`.
It exposes no public names, so each module has to be imported explicitly like this:

```python
Fro mlatex_access import table
```
Code is currently compatible with Python 2.7 and all reasonably recent versions of Python 3, and we intent to keep it that way.
While no modules and sub-packages in latex-access are marked as private, only code documented here should be used by external callers.
The remaining modules are kept public for backwards compatibility only.
Of course if your code relies on one of the modules not described here, issues describing your use cases, or even better PR's with modifications to this guide, are very welcome.

## Support for tables and matrices

Latex-access improves accessibility of LaTeX tables and matrices by means of the `latex_access.table` and `latex_access.matrix_processor` modules respectively.
They have pretty reasonable docstrings, so consult them for their documentation.
<details>
  <summary> TODO: </summary>
    describe them anyway to keep everything in one place.
</details>

