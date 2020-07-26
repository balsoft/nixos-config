{ config, ... }:
let ec = "${config.home-manager.users.balsoft.programs.emacs.finalPackage}/bin/emacsclient";
in
''
  [[ $BLOCK_BUTTON -eq 2 ]] && ${ec} --eval "(org-clock-out)" > /dev/null
  ${ec} --eval "org-mode-line-string" | head -1 | cut -d\" -f 2
''
