#This file defines a com object wihch can be used to interface between 
#screenreaders and the latex access scripts

import latex_access
import nemeth


class latex_access_com:
    _reg_clsid_ = "{436BC4EC-405C-49ED-A0E7-84945B0BAC03}"
    _reg_progid_ = "latex_access"
    _public_methods_ =["nemeth","nemeth_add"]
    def nemeth(self, input):
        '''Translates the input into Nemeth Braille.'''
        return latex_access.translate(str(input),nemeth.table)


    def nemeth_add(self,input,translation):
        '''A function to add entries to the nemeth dictionary.'''
        nemeth.table[str(input)]=str(translation)

#Register the object
if __name__=='__main__':
    import pythoncom,win32com.server.register
    latex_access_com._reg_clsid=pythoncom.CreateGuid()
    win32com.server.register.UseCommandLine(latex_access_com)
