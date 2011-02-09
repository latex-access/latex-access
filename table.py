#    table.py
#    A part of the latex-access project at http://latex-access.sourceforge.net/
#    Author: Daniel Dalton <daniel.dalton10@gmail.com>
#    Copyright (C) 2011 Daniel Dalton/latex-access Contributors
#
#    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation;
#    either version 2 of the License, or (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#    See the GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License along with this program; if not, visit <http://www.gnu.org/licenses>

if __name__ == "__main__":
  print "This can only be used as a module, and does nothing when called interactively."
  exit (-1)
  
def BuildHeaderString (text):
  """Create a tupple of headings.

  These headings will be spoken when queried.
  Provide the first row of a table which should represent the column
  headings. This should be of type list or tupple."""

  tableheadings = [] # clear last output 
  text = text.replace("\hline", "") # remove any hline, though there
  # should only be one at most.
  text = text.replace("\n", "") # So we don't miss last col
  text = text.replace("\\\\", "") # put in a new line char at end of
# row
  text = text+"&" # Don't miss the last column 
  while text.find("&") != -1: # column LaTeX separater
    tableheadings.append(text[:text.find("&")]) # next remove last col
    text = text[text.find("&")+1:] # remove last col and separator
  return tableheadings # return tupple of column headings.

def WhereAmI (row, headers):
  """Provides information of current location in table.

  Currently speak the name of column. In future should speak cell
  coordinates and any other useful info (Perhaps first cell in row)"""
  return "Focus is in column "+headers[row.count("&")]

def GetTableTopRow (table):
  """Return the first row of a table.

  This is useful for separating the row headings from the rest of the
  stuff in the table."""

  return table[:table.find("\\\\")]

def GetTableCurrentRow (table):
  """Return the currently focused row in table.

  Find the last \\ char from end of  input. Everything between end [-1],
  and the last \\ char is considered the current row."""

  return table[table.rfind("\\\\")+1:].replace("\n", "").replace("\\hline",
                                                               "") 
