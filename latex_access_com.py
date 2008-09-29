#This file defines a com object wihch can be used to interface between 
#screenreaders and the latex access scripts


import preprocessor
import nemeth
import speech

class latex_access_com:
    def __init__(self):
        self.nemeth_translator=nemeth.nemeth()
        self.speech_translator=speech.speech()
    _reg_clsid_ = "{436BC4EC-405C-49ED-A0E7-84945B0BAC03}"
    _reg_progid_ = "latex_access"
    _public_methods_ =["nemeth","speech","preprocessor_add","load_csv","toggle_dollars_nemeth","toggle_dollars_speech"]
    def nemeth(self, input):
        '''Translates the input into Nemeth Braille.'''
        input=preprocessor.process(str(input))
        return self.nemeth_translator.translate(input)
    
    def speech(self,input):
        '''Translates the input into english speech.'''
        input=preprocessor.process(str(input))
        return self.speech_translator.translate(input)

    def toggle_dollars_nemeth(self):
        '''Toggles whether dollars are shown in braille.

        Returns a boolian of whether dollars are being removed.'''
        if(self.nemeth_translator.remove_dollars):
            self.nemeth_translator.remove_dollars=False
            return False
        else:
            self.nemeth_translator.remove_dollars=True
            return True

    def toggle_dollars_speech(self):
        '''Toggles whether dollars are spken 

        Returns a boolian of whether dollars are being removed.'''
        if(self.speech_translator.remove_dollars):
            self.speech_translator.remove_dollars=False
            return False
        else:
            self.speech_translator.remove_dollars=True
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
