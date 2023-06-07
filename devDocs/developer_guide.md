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

## Loading user settings

Latex-access has some settings, which are set independently from the screen reader in use.
They should be respected by your interface, since user may use latex-access with several different screen readers, or, as is the case under Linux, with different program for speech and Braille output at the same time.
The format of the config file is documented in the sample configuration file called `latex-access.conf` in the root of the repository, so this section would only describe how to access, and load configuration programmatically.
All functions and variables described in this section are placed in the `latex_access.settings` module, and therefore this prefix would be omitted for brevity.
To load user settings from a  file, you should call `loadSettings` with a path to the file in which latex-access configuration for the current user should be placed.
The function returns `True` when settings are loaded successfully, or `False` if the specified file does not exist, or could not be opened for some reason.
While you may want to alert user to the fact that settings could not be loaded, the library is functional in that case - default settings are used.

<details>

  <summary> Refactoring notes: </summary>
  
  * When loading settings empty lines are ignored only for file with LF (Unix line endings) - this should be fixed.
  * When adding  settings values to the dictionary the values are converted to lower  case, and everything after the first space is ignored. This is problematic for file names on case-sensitive file systems, and for file paths containing spaces.
</details>

The definition of the default settings are in the dictionary `latex_access.settings.settings`, you may want to inspect it to see what values are the default.
Note that this dictionary is changed in place, so after user settings are loaded, it reflects the content of the user settings file, and no longer the default configuration.
This dictionary is intentionally public, to allow making temporary changes to settings from your application.
As to when you should modify it that depends on what effect you want to achieve:
* If you want to configure some setting, but let user preference override the choice, you should set them before calling `loadSettings` - in that case your setting would be used if and only if given setting is not present in the user level configuration file.
* If you want to enforce particular setting (perhaps you have a configuration GUI in your application, or you offer a command line options to set some parameters) then you can modify the `settings` dictionary after settings are loaded. Note that removing values from the map is not supported, and it is assumed that all default keys are always in the dictionary.

User settings are then applied to a particular translators (classes which convert a line of LaTeX to a different representation).
Applying of these settings is done by passing translator instances to the ` activateSettings` function, however these instances must be created first.
To do this you should:
* Create preprocessor instance as described above
* Create instance of a speech translator requested by the user by calling ` get_configured_speech_translator`
* Create instance of Braille translator requested by the user by calling ` brailleTableToUse`

Note that these functions don't warn when the configured translator does not exist- instance of the default Braille and speech translator  is returned.

<details>
<summary> Todo:</summary>
In future they probably should write a warning to a logger - raising exception in that case seems too drastic.
</details>

`activateSettings` accepts a dictionary which should be constructed as follows:
```python
{
    "preprocessor": your_preprocessor_instance,
    "braille": your_braille_translator_instance,
    "speak": your_speech_translator_instance
}
```
If your application has no use for a particular translator (perhaps it only supports translating to Braille) any of these keys can be omitted from the dictionary.
`activateSettings` always returns `True`, so its return value can be safely ignored.
Settings are applied to the passed instances, so they are configured according to the user expectations, and ready to use when translating.
Note that instances you have configured should then be used throughout your  application and you should not re-create them for every translation as that would be extremely wasteful.

<details>
<summary> Refactoring notes: </summary>

This process is way too complicated.
Ideally we would just have a `Settings` class which accepts path to the file name, and can return all configured instances when requested.
It may eventually allow to overwrite some settings (that seems to be required by the BRLTTY table, which allows to specify  Braille translator from the CLI) and pass different preprocessor instance )I'm not sure if re-using preprocessor created earlier should be a supported use case or not).
It should also maintain list of default settings, so that we would not have to repeat the default values for speech and Braille translator in the code.
An alternative, though not mutually exclusive to the above, idea would be to place responsibility for loading settings to a translator base classes, by creating base `SpeechTranslator` and `BrailleTranslator` classes, and adding to them a  method like `configure_from_user_settings` (obviously preprocessor should be threated similarly).
</details>
