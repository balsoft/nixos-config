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
}