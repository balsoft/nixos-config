{ pkgs, config, lib, inputs, ... }:
let
  thm = config.themes.colors;
  thm' = builtins.mapAttrs (name: value: builtins.substring 1 7 value) thm;
  materia_colors = pkgs.writeTextFile {
    name = "gtk-generated-colors";
    text = ''
      BG=${thm'.bg}
      FG=${thm'.fg}
      BTN_BG=${thm'.bg}
      BTN_FG=${thm'.fg}
      MENU_BG=${thm'.bg}
      MENU_FG=${thm'.fg}
      ACCENT_BG=${thm'.blue}
      SEL_BG=${thm'.blue}
      SEL_FG=${thm'.bg}
      TXT_BG=${thm'.bg}
      TXT_FG=${thm'.fg}
      HDR_BTN_BG=${thm'.bg}
      HDR_BTN_FG=${thm'.fg}
      WM_BORDER_FOCUS=${thm'.blue}
      WM_BORDER_UNFOCUS=${thm'.alt}
      MATERIA_STYLE_COMPACT=True
      MATERIA_COLOR_VARIANT=dark
      UNITY_DEFAULT_LAUNCHER_STYLE=False
      NAME=generated
    '';
  };
in {
  nixpkgs.overlays = [(self: super: {
    generated-gtk-theme = self.stdenv.mkDerivation rec {
      name = "generated-gtk-theme";
      src = inputs.materia-theme;
      buildInputs = with self; [ sassc bc which inkscape optipng ];
      installPhase = ''
        HOME=/build
        chmod 777 -R .
        patchShebangs .
        mkdir -p $out/share/themes
        substituteInPlace change_color.sh --replace "\$HOME/.themes" "$out/share/themes"
        echo "Changing colours:"
        ./change_color.sh -o Generated ${materia_colors}
        chmod 555 -R .
      '';
    };
  })];
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  home-manager.users.balsoft = {
    gtk = {
      enable = true;
      iconTheme = {
        name = "Papirus-Dark";
        package = pkgs.papirus-icon-theme;
      };
      theme = {
        name = "Generated";
        package = pkgs.generated-gtk-theme;
      };
      font = {
        name = "IBM Plex 12";
      };
      gtk3.extraConfig.gtk-cursor-theme-name = "breeze";
    };
    home.sessionVariables.GTK_THEME = "Generated";
  };
}
