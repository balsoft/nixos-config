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
        out = subprocess.check_output(["nix", "eval", "--raw", "%s" % (query.string or '("type a nix expression (nixpkgs are in scope)")')])
        return [
            Item(
                id = "nix",
                icon = iconPath,
                text = out,
                completion = query.rawString,
                actions=[ClipAction('Copy to clipboard', out)]
            )
        ]
