import latex_access
import latex_access.latex_access
try:
    from latex_access import speech_modified
except ImportError:
    latex_access.get_arg = latex_access.latex_access.get_arg
    latex_access.translator = latex_access.latex_access.translator
    from latex_access import speech_modified
    speech_modified.latex_access = latex_access.latex_access
    del latex_access.get_arg
    del latex_access.translator
