#!/usr/bin/env python3

import json, subprocess, sys

root = json.loads(subprocess.check_output(["swaymsg", "-t", "get_tree"]))

class Window:
  def __init__(self, raw_output, raw_workspace, raw_window):
    self.window = raw_window
    self.workspace = raw_workspace
    self.output = raw_output
    
  def focus(self):
    subprocess.check_output(["swaymsg", "[con_id=%s] focus" % self.window["id"]])

  def __str__(self):
    return "'%s' %s/%s on %s" % (self.window["name"], self.window["id"], self.window["app_id"], self.workspace["name"])


def find_windows(name):
  for output in root["nodes"]:
    for workspace in output["nodes"]:
      if "nodes" not in workspace.keys(): continue
      for window in workspace["nodes"]:
        if window["name"].lower().count(name.lower()) > 0:
          yield Window(output, workspace, window)


if __name__ == "__main__":
  windows = list(find_windows(sys.argv[1] if len(sys.argv) == 2 else ""))
  for window in windows:
    print(window)

