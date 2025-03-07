{ pkgs, config, inputs, ... }:
let
  thm = config.themes.colors;
  thm' = builtins.mapAttrs (name: value: { hex.rgb = value; }) thm;
in {
  programs.gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
  nixpkgs.overlays = [
    (self: super: {
      generated-gtk-theme =
        pkgs.callPackage "${inputs.rycee}/pkgs/materia-theme" {
          configBase16 = {
            name = "Generated";
            kind = "dark";
            colors = thm' // {
              base01 = thm'.base00;
              base02 = thm'.base00;
            };
          };
        };
    })
  ];
  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ dconf ];
  home-manager.users.balsoft = {
    gtk = {
      enable = true;
      iconTheme = {
        name = "breeze-dark";
        package = pkgs.kdePackages.breeze-icons;
      };
      theme = {
        name = "Generated";
        package = pkgs.generated-gtk-theme;
      };
      font = {
        name = with config.themes.fonts; "${main.family} ${toString main.size}";
      };
      gtk3 = {
        bookmarks = [
          "file:///home/balsoft/projects Projects"
          "davs://nextcloud.balsoft.ru/remote.php/dav/files/balsoft nextcloud.balsoft.ru"
          "sftp://balsoft.ru/home/balsoft balsoft.ru"
        ] ++ map (machine: "sftp://${machine}/home/balsoft ${machine}")
          (builtins.attrNames inputs.self.nixosConfigurations);
      };

    };
    home.sessionVariables.GTK_THEME = "Generated";
  };
}
