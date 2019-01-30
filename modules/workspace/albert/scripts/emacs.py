# -*- coding: utf-8 -*-

"""
Opens emacs directly in ~/projects, running in nix-shell if needed
"""

import os
from shutil import which

from albertv0 import *

__iid__ = "PythonInterface/v0.1"
__prettyname__ = "emacs"
__version__ = "1.0"
__trigger__ = "em "
__author__ = "Alexander Bantyev"
__dependencies__ = ["emacs", "nix-shell"]

if which("emacs") is None or which("nix-shell") is None:
    raise Exception("'emacs' is not in $PATH.")

def handleQuery(query):
    if query.isTriggered:
       results = []
       path = os.environ["HOME"] + "/projects/"
       subDirList = list(filter(lambda x: os.path.isdir(x), map(lambda x: path + x, os.listdir(path))))
       for dirName, fileList in [(x, os.listdir(x)) for x in subDirList]:
           if dirName.find(query.string) == -1:
               continue
           if "default.nix" in fileList or "shell.nix" in fileList:
               results.append(
                   Item(
                       id = "emacs%s" % dirName,
                       text = "Open emacs in %s" % dirName,
                       subtext = "(in nix-shell)",
                       actions = [ ProcAction("Open emacs in nix-shell", ["sh", "-c", "cd '%s' && nix-shell --run 'emacs .'" % dirName]) ]
                       )
               )
           else:
               results.append(
                   Item(
                       id = "emacs%s" % dirName,
                       text = "Open emacs in %s" % dirName,
                       actions = [ ProcAction("Open emacs", ["sh", "-c", "cd '%s' && emacs ." % dirName]) ]
                       )
               )
       return results
