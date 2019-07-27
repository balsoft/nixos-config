{ pkgs, ... }: {
  home-manager.users.balsoft = {
    xresources.properties = {
      "st.shell" = "${pkgs.zsh}/bin/zsh";
      "st.cursorshape" = "5";
      "st.xfps" = "1000";
      "st.actionfps" = "1000";
      "st.font" = "Roboto Mono:pixelsize=14:antialias=true:autohint=true;";
    };
  };
}
