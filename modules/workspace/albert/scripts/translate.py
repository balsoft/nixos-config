# -*- coding: utf-8 -*-
"""Translate text using translate-shell"""
import subprocess
from albertv0 import *

__iid__ = "PythonInterface/v0.1"
__prettyname__ = "custom translate-shell interface"
__version__ = "1.0"
__trigger__ = "tr "
__author__ = "Alexander Bantyev"
__dependencies__ = []
iconPath = iconLookup('config-language')
if not iconPath:
    iconPath = ":python_module"
def handleQuery(query):
    if query.isTriggered:
        if query.string == "":
            return []
        result = subprocess.getoutput("echo "" | trans -b %s" % query.string)
        return [
            Item(
                id = "translate",
                icon = iconPath,
                text = result,
                actions=[ClipAction('Copy to clipboard', result)]
            )
        ]
