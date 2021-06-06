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
      ACCENT_BG=${thm'.alt}
      SEL_BG=${thm'.blue}
      SEL_FG=${thm'.bg}
      TXT_BG=${thm'.bg}
      TXT_FG=${thm'.fg}
      HDR_BTN_BG=${thm'.bg}
      HDR_BTN_FG=${thm'.fg}
      WM_BORDER_FOCUS=${thm'.alt}
      WM_BORDER_UNFOCUS=${thm'.dark}
      MATERIA_STYLE_COMPACT=True
      MATERIA_COLOR_VARIANT=dark
      UNITY_DEFAULT_LAUNCHER_STYLE=False
      NAME=generated
    '';
  };
in {
  nixpkgs.overlays = [
    (self: super: {
      rendersvg = self.runCommandNoCC "rendersvg" {} ''
        mkdir -p $out/bin
        ln -s ${self.resvg}/bin/resvg $out/bin/rendersvg
      '';
      generated-gtk-theme = self.stdenv.mkDerivation rec {
        name = "generated-gtk-theme";
        src = inputs.materia-theme;
        buildInputs = with self; [ sassc bc which rendersvg meson ninja nodePackages.sass gtk4.dev optipng ];
        MATERIA_COLORS = materia_colors;
        phases = [ "unpackPhase" "installPhase" ];
        installPhase = ''
          HOME=/build
          chmod 777 -R .
          patchShebangs .
          mkdir -p $out/share/themes
          mkdir bin
          sed -e 's/handle-horz-.*//' -e 's/handle-vert-.*//' -i ./src/gtk-2.0/assets.txt
          echo "Changing colours:"
          ./change_color.sh -o Generated "$MATERIA_COLORS" -i False -t "$out/share/themes"
          chmod 555 -R .
        '';
      };
    })
  ];
  programs.dconf.enable = true;
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
      font = { name = "IBM Plex 12"; };
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
