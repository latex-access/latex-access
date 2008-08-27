#This file defines a com object wihch can be used to interface between 
#screenreaders and the latex access scripts

import latex_access
import preprocessor
import nemeth
import speech

class latex_access_com:
    _reg_clsid_ = "{436BC4EC-405C-49ED-A0E7-84945B0BAC03}"
    _reg_progid_ = "latex_access"
    _public_methods_ =["nemeth","speech","preprocessor_add","load_csv","toggle_dollars_nemeth","toggle_dollars_speech"]
    def nemeth(self, input):
        '''Translates the input into Nemeth Braille.'''
        input=preprocessor.process(str(input))
        return latex_access.translate(input,nemeth.table)
    
    def speech(self,input):
        '''Translates the input into english speech.'''
        input=preprocessor.process(str(input))
        return latex_access.translate(input,speech.table)

    def toggle_dollars_nemeth(self):
        '''Toggles whether dollars are shown in braille.

        Returns a boolian of whether dollars are being removed.'''
        if(nemeth.remove_dollars):
            nemeth.remove_dollars=False
            return False
        else:
            nemeth.remove_dollars=True
            return True

    def toggle_dollars_speech(self):
        '''Toggles whether dollars are spken 

        Returns a boolian of whether dollars are being removed.'''
        if(speech.remove_dollars):
            speech.remove_dollars=False
            return False
        else:
            speech.remove_dollars=True
            return True

    def preprocessor_add(self,input,translation):
        '''A function to add entries to the preprocessor'''
        preprocessor.table[str(input)]=str(translation)

    def load_csv(self,file):
        '''Load a csv file into the preprocessor'''
        preprocessor.load_csv(str(file))


#Register the object
if __name__=='__main__':
    import pythoncom,win32com.server.register
    latex_access_com._reg_clsid=pythoncom.CreateGuid()
    win32com.server.register.UseCommandLine(latex_access_com)
