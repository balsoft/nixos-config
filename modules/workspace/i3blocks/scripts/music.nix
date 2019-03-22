{ bash, playerctl, iconfont, ... }:
''
#!${bash}/bin/bash
STATUS=`${playerctl}/bin/playerctl status`
case $BLOCK_BUTTON in
     1) ${playerctl}/bin/playerctl play-pause;;
     2) ${playerctl}/bin/playerctl stop;;
     3) ${playerctl}/bin/playerctl next;;
esac
case $STATUS in
     Paused)  icon=""; text=`${playerctl}/bin/playerctl metadata title | head -c 20 | sed s/' \\w*$'//`;;
     Playing) icon=""; text=`${playerctl}/bin/playerctl metadata title | head -c 30 | sed s/' \\w*$'//`;;
     *) icon="";;
esac
echo "<span font='${iconfont}'>$icon</span> $text"
''
