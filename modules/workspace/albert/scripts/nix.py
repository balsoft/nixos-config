""" Nix Repl interacive execution """
import subprocess
from albertv0 import *
__iid__ = "PythonInterface/v0.1"
__prettyname__ = "Nix module"
__version__ = "0.1"
__author__ = "Alexander Bantyev"
__trigger__ = "nix "
__dependencies__ = []
iconPath = iconLookup("nix-snoflake")
def handleQuery(query):
    if query.isTriggered:
        out = subprocess.getoutput("nix repl <<< 'let pkgs=import <nixpkgs> {}; in %s' | sed 's/\[36m//; s/\[33m//; s/\[0m//; s/[[:cntrl:]]//' | tail -3" % query.string)
        return [
            Item(
                id = "nix",
                icon = iconPath,
                text = out,
                completion = query.rawString,
                actions=[ClipAction('Copy to clipboard', out)]
            )
        ]
