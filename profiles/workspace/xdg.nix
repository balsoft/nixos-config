{
  ...
}:
{
  home-manager.users.balsoft = {
    xdg.enable = true;
    xdg.userDirs.enable = true;
    xdg.userDirs.setSessionVariables = true;
  };

  environment.sessionVariables = {
    XDG_CURRENT_DESKTOP = "X-Generic";
    DE = "generic";
  };

  persist.state.directories = map (x: "/home/balsoft/${x}") [
    "Pictures"
    "Documents"
    "Downloads"
    "Music"
    "projects"
    "Videos"
  ];
}
