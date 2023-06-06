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

## Preprocessor
The remaining modules in the library are used to improve readability of math in LaTeX, by translating it to more understandable speech representation, or a Braille notation selected by the user.
While starting their description by discussing preprocessor, which may seem to be more advanced concept, might seems strange at first, understanding how to create preprocessor instances is unfortunately necessary to effectively load user settings.
The main purpose of the preprocessor is handling custom user defined commands.
This can be done in two ways: either by adding them manually, or by extracting their arguments from the `\newcommand` or `\renewcommand` definitions.
The preprocessor module contains two classes:
* `preprocessor` - basic preprocessor interface which allows to add a new command manually, save / restore added definitions and convert LaTeX containing new user defined commands to their original form.
It should be used as follows:
    * Create a single instance of `preprocessor.preprocessor`, which should be used throughout your application
    * You should provide an interface for adding custom commands - it should allow to enter the same arguments as accepted by the `preprocessor. add_from_string` method
    <details>
      <summary> Todo: </summary>
        document them!
    </details>

    * Since by default preprocessor entries are saved in memory, if  you want to make them persistent, an interface for specifying path to the file in which they would be saved should be offered. After the path is entered by the user, you should call the `write` method, providing it absolute path to the file
    * To load saved entries, you should call the `read` method, again with an absolute path to the file
    * Before translating a line of LaTeX to speech or Braille you should pass it to the preprocessor's `translate` method, and only then translate the returned value further.
    This ensures that user defined commands are expanded to their full form.

    <details>
      <summary> Refactoring notes: </summary>
      <ul>
        <li> Preprocessor has an <code>add</code> method which appears to be unused. Confirm and remove it.</li>
        <li> If translator would ever be divided to interface and a base implementation preprocessor's initializer should be removed, and it should inherit from the basic interface (it has no need for methods for translating text, visual formatting commands etc.).</li>
      </ul>
    </details>

* `newcommands` - - this class is used to automatically create translation of user defined commands, by extracting their definitions from the `\
newcommand` and `\renewcommand` declarations.
It should be used as follows:
    * Once again create a single instance, and pass the preprocessor you are using to its initializer
    * To add a definition of the custom command you should pass a string with the `\newcommand` or `\renewcommand` to the newcommand's `translate` method. The returned value should be ignored. As a result the definition of the command is added to your main preprocessor, which is stored by the newcommand as an instance variable. (composition is awesome isn't it?). 
<details>
  <summary> Refactoring notes: </summary>
    As above newcommands should inherit from the translator's interface, not from the implementation, since mapping of all translating methods is removed in the initializer anyway.
</details>
