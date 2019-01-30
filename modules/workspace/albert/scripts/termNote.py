# -*- coding: utf-8 -*-

"""
termNote note manager support
"""

import subprocess
import re
import sys
from collections import namedtuple
from shutil import which

from albertv0 import *

__iid__ = "PythonInterface/v0.1"
__prettyname__ = "termNote support"
__version__ = "1.0"
__trigger__ = "note"
__author__ = "Alexander Bantyev"
__dependencies__ = ["termNote"]

if which("termNote") is None:
    raise Exception("'termNote' is not in $PATH.")

def handleQuery(query):
    if query.isTriggered:
       results = [
           Item(
               id = "termNoteAdd",
               text = "Add a note",
               actions = [ProcAction("Add a note", ["termNote", "-a", query.string])]
               )
       ]
       for line in subprocess.check_output(['termNote'] + query.string.split()).splitlines():
           i = int(re.search("\[([0-9]+)\]", str(line)).group(1))
           results.append(Item(
               id = "termNoteLine%s" % i,
               text = line,
               actions = [
                   ClipAction("Copy to clipboard", subprocess.check_output(["termNote", "-s", str(i)]).splitlines()[0]),
                   ProcAction("Complete note", ["termNote", "-c", str(i)])
               ]
           ))
       return results
