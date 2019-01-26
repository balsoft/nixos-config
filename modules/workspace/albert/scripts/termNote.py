# -*- coding: utf-8 -*-

"""
termNote note manager support
"""

import subprocess
from collections import namedtuple
from shutil import which

from albertv0 import *

__iid__ = "PythonInterface/v0.1"
__prettyname__ = "termNote support"
__version__ = "1.0"
__trigger__ = "termNote"
__author__ = "Alexander Bantyev"
__dependencies__ = ["termNote"]

if which("termNote") is None:
    raise Exception("'termNote' is not in $PATH.")

def handleQuery(query):
    if query.isTriggered:
       results = []
       for line in subprocess.check_output(['termNote'] + query.string.split()).splitlines():
           
       return results
