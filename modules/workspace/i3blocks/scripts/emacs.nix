{...}: ''
  emacsclient --eval "org-mode-line-string" | head -1 | cut -d\" -f 2
''
