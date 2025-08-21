{ pkgs, lib, ... }: {
  options = with lib;
    with types; {
      defaultApplications = mkOption {
        type = attrsOf (submodule ({ name, ... }: {
          options = {
            cmd = mkOption { type = path; };
            desktop = mkOption { type = str; };
          };
        }));
        description = "Preferred applications";
      };

      startupApplications = mkOption {
        type = listOf path;
        description = "Applications to run on startup";
      };
    };
  config = {
    defaultApplications = {
      text_processor = {
        cmd = "${pkgs.abiword}/bin/abiword";
        desktop = "abiword";
      };
      spreadsheet = {
        cmd = "${pkgs.gnumeric}/bin/gnumeric";
        desktop = "gnumeric";
      };
    };
  };
}
