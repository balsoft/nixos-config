{ python3, playerctl, iconfont, ...}: ''
  #!${python3}/bin/python
  from subprocess import getoutput, call
  from os import environ
  status = getoutput("${playerctl}/bin/playerctl status")
  if "BLOCK_BUTTON" in environ:
    BLOCK_BUTTON = int(environ["BLOCK_BUTTON"])
    if BLOCK_BUTTON == 1: call(["${playerctl}/bin/playerctl", "play-pause"])
    if BLOCK_BUTTON == 2: call(["${playerctl}/bin/playerctl", "stop"])
    if BLOCK_BUTTON == 3: call(["${playerctl}/bin/playerctl", "next"])
  text = getoutput("${playerctl}/bin/playerctl metadata title")[:20:]
  if status == "Paused": 
    icon=""
  elif status == "Playing": 
    icon=""
  else: 
    icon = ""
  print("<span font='${iconfont}'>%s</span> %s" % (icon, text))
''
