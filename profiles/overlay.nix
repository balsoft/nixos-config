{ pkgs, config, lib, inputs, ... }:
let
  filterGit =
    builtins.filterSource (type: name: name != ".git" || type != "directory");
  system = "x86_64-linux";
  old = import inputs.nixpkgs-old ({
    config = config.nixpkgs.config;
    localSystem = { inherit system; };
  });
  mkKeyValue = key: value:
    let
      mvalue = if builtins.isBool value then
        (if value then "true" else "false")
      else if builtins.isString value then
        value
      else
        builtins.toString value;
    in ''${key}=${mvalue}'';
  attrsToList = with builtins;
    x:
    (map (key: {
      name = key;
      value = getAttr key x;
    }) (attrNames x));
in {
  nixpkgs.overlays = [
    (import inputs.emacs-overlay)
    (self: super: rec {

      my-lib = rec {
        genIni = lib.generators.toINI { inherit mkKeyValue; };
        genIniOrdered = lst:
          (builtins.concatStringsSep "\n" (map ({ name ? "widget", ... }@attrs:
            builtins.concatStringsSep "\n" ([ "[${name}]" ]
              ++ (map ({ name, value }: mkKeyValue name value)
                (attrsToList (builtins.removeAttrs attrs [ "name" ]))))) lst))
          + "\n";
        thm = config.themes.colors;
        splitHex = hexStr:
          map (x: builtins.elemAt x 0) (builtins.filter (a: a != "" && a != [ ])
            (builtins.split "(.{2})" hexStr));
        hex2decDigits = rec {
          "0" = 0;
          "1" = 1;
          "2" = 2;
          "3" = 3;
          "4" = 4;
          "5" = 5;
          "6" = 6;
          "7" = 7;
          "8" = 8;
          "9" = 9;
          "a" = 10;
          "b" = 11;
          "c" = 12;
          "d" = 13;
          "e" = 14;
          "f" = 15;
          A = a;
          B = b;
          C = c;
          D = d;
          E = e;
          F = f;
        };

        doubleDigitHexToDec = hex:
          16 * hex2decDigits."${builtins.substring 0 1 hex}"
          + hex2decDigits."${builtins.substring 1 2 hex}";
        thmDec = builtins.mapAttrs (name: color: colorHex2Dec color) thm;
        thmHash = builtins.mapAttrs (name: color: "#${color}") thm;
        colorHex2Dec = color:
          builtins.concatStringsSep ","
          (map (x: toString (doubleDigitHexToDec x)) (splitHex color));

      };

      nur = (import inputs.NUR {
        pkgs = old;
        nurpkgs = pkgs;
      }).repos;

      mopidy-ytmusic = with pkgs;
        python3Packages.buildPythonApplication rec {
          pname = "mopidy-ytmusic";
          version = "3.2";

          src = fetchFromGitHub {
            owner = "ozymandiasthegreat";
            repo = "mopidy-ytmusic";
            rev = "2a7d8ec7f7161e5ca81e999786ea7c1f4da9205f";
            sha256 = "hDOP0AR9P0sQHeW7JmG+yZf/w5mZrIu4Nl4xCMjQaIQ=";
            fetchSubmodules = true;
          };

          propagatedBuildInputs = [
            mopidy
            python3Packages.pytube
            python3Packages.ytmusicapi
          ];

          doCheck = false;
        };

      inherit (nur.balsoft.pkgs) termNote nix-patch;

      lambda-launcher = inputs.lambda-launcher.defaultPackage.${system};

      simple-osd = inputs.simple-osd-daemons.packages.${system};

      yt-utilities = inputs.yt-utilities.defaultPackage.${system};

      nerdfonts = nur.balsoft.pkgs.roboto-mono-nerd;

      # inkscape = super.inkscape.overrideAttrs (oa: {
      #   src = inputs.inkscape;
      #   buildInputs = oa.buildInputs ++ [
      #     self.double-conversion
      #   ];
      #   postPatch = ''
      #     patchShebangs share/extensions
      #     patchShebangs share/templates
      #     patchShebangs man/fix-roff-punct
      #   '';
      # });

      nix-direnv = inputs.nix-direnv.defaultPackage.${system};

      mtxclient = super.mtxclient.overrideAttrs (_: {
        src = inputs.mtxclient;
      });

      nheko = (super.nheko.overrideAttrs (oa: {
        src = inputs.nheko;
      })).override { mtxclient = self.mtxclient; };
    })
  ];
  nixpkgs.config = {
    allowUnfree = true;
    android_sdk.accept_license = true;
  };
}
