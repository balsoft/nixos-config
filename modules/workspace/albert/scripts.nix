{ pkgs }:
{
    qalc = ''
    """ Qalc module """
    import subprocess
    from albertv0 import *
    __iid__ = "PythonInterface/v0.1"
    __prettyname__ = "Qalculate! module"
    __version__ = "0.1"
    __author__ = "Alexander Bantyev"
    __dependencies__ = []
    iconPath = iconLookup('accessories-calculator')
    def handleQuery(query):
        if query.string == "":
            return []
        qalc_out = subprocess.getoutput('qalc -s "exact 0" -t "%s"' % query.string)
        qalc_out_exact = subprocess.getoutput('qalc -s "exact 1" -t "%s"' % query.string)
        return [
            Item(
                id="qalc",
                icon=iconPath,
                text=qalc_out_exact,
                subtext=qalc_out if qalc_out != qalc_out_exact else "",
                completion=query.rawString,
            )
        ]
    '';
    nix = ''
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
            return [
                Item(
                    id = "nix",
                    icon = iconPath,
                    text = subprocess.getoutput("nix repl <<< 'let pkgs=import <nixpkgs> {}; in %s' | sed 's/\[36m//; s/\[33m//; s/\[0m//; s/[[:cntrl:]]//' | tail -3" % query.string),
                    completion = query.rawString
                )
            ]
    '';
    translate = ''
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
                    text = result
                )
            ]
    '';
}
