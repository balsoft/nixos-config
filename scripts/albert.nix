{ pkgs }:
{
    qalc = ''
    """ Qalc module """
    import subprocess
    from albertv0 import *
    __iid__ = "PythonInterface/v0.1"
    __prettyname__ = "Qalculate! module"
    __version__ = "0.1"
    __trigger__ = ""
    __author__ = "Alexander Bantyev"
    __dependencies__ = []
    iconPath = iconLookup('calculator')
    def handleQuery(query):
        qalc_out = subprocess.getoutput('${pkgs.libqalculate}/bin/qalc -s "exact 0" "%s"' % query.string)
        qalc_out_exact = subprocess.getoutput('${pkgs.libqalculate}/bin/qalc -s "exact 1" "%s"' % query.string)
        if "error" in qalc_out or "=" not in qalc_out:
            return []
        return [
            Item(
                id="qalc",
                icon=iconPath,
                text=qalc_out_exact.split(" = ")[-1],
                subtext=qalc_out.split(" = ")[-1] if qalc_out.split(" = ")[-1] != qalc_out_exact.split(" = ")[-1] else qalc_out,
                completion=query.rawString,
            )
        ]
    '';
}