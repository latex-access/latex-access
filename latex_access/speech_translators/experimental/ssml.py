# ssml.py
#    A part of the latex-access project at http://latex-access.sourceforge.net/
#    Author: Alastair Irving <alastair.irving@sjc.ox.ac.uk>
#    Copyright (C) 2011 Alastair Irving/latex-access Contributors
#
#    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation;
#    either version 2 of the License, or (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#    See the GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License along with this program; if not, visit <http://www.gnu.org/licenses>
#
'''Experimental Module to provide speech output in SSML for latex_access.'''

from __future__ import absolute_import

import os.path

from latex_access.speech_translators import speech as base_translator
from latex_access.latex_access import get_arg

sb='<break strength="strong">'


class speech(base_translator.speech):
    def __init__(self):
        base_translator.speech.__init__(self)
        self.table["_"]=('<prosody pitch="-15%">','</prosody>')

    def sqrt(self,input,start):
        '''Translates squareroots into speech.
        
        returns touple.'''
        arg=get_arg(input,start)
        if arg[0].isdigit() or len(arg[0])==1:
            translation=" root "+arg[0]
        else:
            translation=" begin root "+sb+self.translate(arg[0])+" end root "+sb
        return (translation,arg[1])

    def get_buildin_tables_dir(self):
        """Since this translator uses table of its base class let's return path to the parent directory"""
        return os.path.join(base_translator.speech.get_buildin_tables_dir(self), "..")
