{ pkgs, config, lib, inputs, ... }:
let
  filterGit =
    builtins.filterSource (type: name: name != ".git" || type != "directory");
  inherit (pkgs) system;
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

          src = inputs.mopidy-ytmusic;

          propagatedBuildInputs = [
            mopidy
            (python3Packages.ytmusicapi.overrideAttrs (oa: rec {
              name = "python3.9-ytmusicapi-${version}";
              version = "0.19.1";
              src = fetchFromGitHub {
                owner = "sigma67";
                repo = "ytmusicapi";
                rev = "fd9f57750de103202106f02be1696bd440f2c05b";
                sha256 = "/NMy2cGe0K/14OZd+/dXKA6Ez1ivrtrZ6Lwl0P8dioA=";
                fetchSubmodules = true;
              };
            }))
            (python3Packages.pytube.overrideAttrs (oa: rec {
              name = "python3.9-pytube-${version}";
              version = "11.0.1";
              src = fetchFromGitHub {
                owner = "pytube";
                repo = "pytube";
                rev = "f06e0710dcf5089e582487fee94f7bb0afbf7ba9";
                sha256 = "sha256-yQCgrnoPOSdTnTPEsVkgLYpPLiHq7kXRUO72TxD152k=";
                fetchSubmodules = true;
              };
            }))
          ];

          doCheck = false;
        };

      inherit (nur.balsoft.pkgs) termNote nix-patch;

      lambda-launcher = inputs.lambda-launcher.defaultPackage.${system};

      simple-osd = inputs.simple-osd-daemons.packages.${system};

      nerdfonts = nur.balsoft.pkgs.roboto-mono-nerd;

      pass-secret-service = super.pass-secret-service.overrideAttrs (_: { installCheckPhase = null; });

      nix-direnv = inputs.nix-direnv.defaultPackage.${system};

      coeurl = self.stdenv.mkDerivation {
        name = "coeurl";
        src = inputs.coeurl;
        buildInputs = [ self.curl.all self.libevent self.spdlog ];
        nativeBuildInputs = [ self.meson self.ninja self.pkg-config self.cmake ];
      };

      mtxclient = super.mtxclient.overrideAttrs (oa: {
        src = inputs.mtxclient;
        cmakeFlags = oa.cmakeFlags ++ [ "-DCMAKE_CXX_FLAGS=-DSPDLOG_FMT_EXTERNAL" ];
        buildInputs = oa.buildInputs ++ [ self.libevent self.curl.all self.coeurl self.spdlog.dev ];
        patches = [];
      });

      nheko = (super.nheko.overrideAttrs (oa: {
        src = inputs.nheko;
        postPatch = ''
          substituteInPlace CMakeLists.txt --replace "# Fixup bundled keychain include dirs" "find_package(Boost COMPONENTS iostreams system  thread REQUIRED)"
        '';
        buildInputs = oa.buildInputs ++ [
          self.xorg.libXdmcp
          self.pcre
          self.libunwind
          self.elfutils
          self.coeurl
          self.curl
          self.libevent
          self.asciidoc
        ];
        cmakeFlags = oa.cmakeFlags ++ [ "-DBUILD_SHARED_LIBS=OFF" ];
      })).override { mtxclient = self.mtxclient; };

      nix = inputs.nix.defaultPackage.${pkgs.system}.overrideAttrs (oa: {
        patches = [ ./nix/nix.patch ] ++ oa.patches or [ ];
      });

    })
  ];
  nixpkgs.config = {
    allowUnfree = true;
    android_sdk.accept_license = true;
  };
}
