# -*- coding: utf-8 -*-

"""
X11 window switcher - list and activate windows.
"""

import subprocess


from albertv0 import *

import json, subprocess, sys

class Window:
    def __init__(self, raw_output, raw_workspace, raw_window):
        self.window = raw_window
        self.workspace = raw_workspace
        self.output = raw_output
      
    def run_command(self, command):
        subprocess.check_output(["swaymsg", "[con_id=%s] %s" % (self.window["id"], command)])

    def focus(self):
        self.run_command("focus")
 
    def kill(self):
        self.run_command("kill")

    def __str__(self):
        return "'%s' %s/%s on %s" % (self.window["name"], self.window["id"], self.window["app_id"], self.workspace["name"])


def find_windows(root, name):
    for output in root["nodes"]:
        for workspace in output["nodes"]:
            if "nodes" not in workspace.keys(): continue
            for window in workspace["nodes"]:
                if "name" not in window.keys(): continue
                if window["name"].lower().count(name.lower()) > 0:
                    yield Window(output, workspace, window)




__iid__ = "PythonInterface/v0.1"
__prettyname__ = "Window Switcher"
__version__ = "1.3"
__trigger__ = "w"
__author__ = "Ed Perez, Manuel Schneider, Alexander Bantyev"


def handleQuery(query):
    if not query.isTriggered:
        return []
    windows = find_windows(json.loads(subprocess.check_output(["swaymsg", "-t", "get_tree"])), query.string or "")
    items = []
    for window in windows:
        items.append(Item(id="sway"+str(window.window["id"]),
                          text=window.window["name"],
                          subtext=window.workspace["name"] + "/" + window.output["name"],
                          actions = [FuncAction("Focus the window", window.focus), FuncAction("Close the window", window.kill)]
        ))
        
    return items
