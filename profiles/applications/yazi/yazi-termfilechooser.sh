#!/bin/sh
# This wrapper script is invoked by xdg-desktop-portal-termfilechooser.
#
# For more information about input/output arguments read `xdg-desktop-portal-termfilechooser(5)`

multiple="$1"
directory="$2"
save="$3"
initialpath="$4"
out="$5"
debug="$6"

set -e

cmd="yazi"
termcmd="${TERMCMD:-"xdg-terminal-exec"}"

if [ "$save" = "1" ]; then
    # save a file
    set -- --chooser-file="$out" "$initialpath"
elif [ "$directory" = "1" ]; then
    # upload files from a directory
    set -- --chooser-file="$out" --cwd-file="$out"".1" "$initialpath"
elif [ "$multiple" = "1" ]; then
    # upload multiple files
    set -- --chooser-file="$out" "$initialpath"
else
    # upload only 1 file
    set -- --chooser-file="$out" "$initialpath"
fi

command="$termcmd $cmd"
for arg in "$@"; do
    # escape double quotes
    escaped=$(printf "%s" "$arg" | sed 's/"/\\"/g')
    # escape special
    command="$command \"$escaped\""
done

sh -c "$command"

if [ "$directory" = "1" ]; then
    if [ ! -s "$out" ] && [ -s "$out"".1" ]; then
        cat "$out"".1" > "$out"
        rm "$out"".1"
    else
        rm "$out"".1"
    fi
elif [ "$save" = "1" ]; then
    if [ -d "$(cat "$out")" ]; then
        dir="$(cat "$out")"
        file="${initialpath##*/}"
        fullpath="$dir/$file"
        touch "$fullpath"
        printf "%s" "$fullpath" > "$out"
    fi
fi
