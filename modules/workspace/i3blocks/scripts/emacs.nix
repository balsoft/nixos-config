{...}: ''
  emacsclient --eval "(print org-mode-line-string)" | head -1 | cut -d\" -f 2
''
