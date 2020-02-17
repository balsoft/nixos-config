{...}: ''
  [[ $BLOCK_BUTTON -eq 2 ]] && emacsclient --eval "(org-clock-out)" > /dev/null
  emacsclient --eval "org-mode-line-string" | head -1 | cut -d\" -f 2
''
