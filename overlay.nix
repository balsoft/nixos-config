inputs: final: prev:
let
  old =
    import inputs.nixpkgs-old ({ localSystem = { inherit (final) system; }; });
  inherit (final) system lib;
in rec {

  my-lib = import ./lib.nix lib;

  nur = (import inputs.NUR {
    pkgs = old;
    nurpkgs = final;
  }).repos;

  mopidy-ytmusic = with final;
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

  pass-secret-service =
    prev.pass-secret-service.overrideAttrs (_: { 
      installCheckPhase = null;
      postInstall = ''
        mkdir -p $out/share/{dbus-1/services,xdg-desktop-portal/portals}
        cat > $out/share/dbus-1/services/org.freedesktop.secrets.service << EOF
        [D-BUS Service]
        Name=org.freedesktop.secrets
        Exec=/run/current-system/sw/bin/systemctl --user start pass-secret-service
        EOF
        cp $out/share/dbus-1/services/{org.freedesktop.secrets.service,org.freedesktop.impl.portal.Secret.service}
        cat > $out/share/xdg-desktop-portal/portals/pass-secret-service.portal << EOF
        [portal]
        DBusName=org.freedesktop.secrets
        Interfaces=org.freedesktop.impl.portal.Secrets
        UseIn=gnome
        EOF
      '';
    });

  nix-direnv = inputs.nix-direnv.packages.${system}.default.override { pkgs = final; };

  # For nix-direnv
  nixFlakes = final.nix;

  coeurl = final.stdenv.mkDerivation {
    name = "coeurl";
    src = inputs.coeurl;
    buildInputs = [ final.curl.all final.libevent final.spdlog ];
    nativeBuildInputs =
      [ final.meson final.ninja final.pkg-config final.cmake ];
  };

  mtxclient = prev.mtxclient.overrideAttrs (oa: {
    src = inputs.mtxclient;
    cmakeFlags = oa.cmakeFlags ++ [ "-DCMAKE_CXX_FLAGS=-DSPDLOG_FMT_EXTERNAL" ];
    buildInputs = oa.buildInputs
      ++ [ final.libevent final.curl.all final.coeurl final.spdlog.dev final.re2 ];
    patches = [ ];
  });

  nheko = (prev.nheko.overrideAttrs (oa: {
    src = inputs.nheko;
    postPatch = ''
      substituteInPlace CMakeLists.txt --replace "# Fixup bundled keychain include dirs" "find_package(Boost COMPONENTS iostreams system  thread REQUIRED)"
    '';
    buildInputs = oa.buildInputs ++ [
      final.xorg.libXdmcp
      final.pcre
      final.libunwind
      final.elfutils
      final.coeurl
      final.curl
      final.libevent
      final.asciidoc
      final.re2
    ];
    cmakeFlags = oa.cmakeFlags ++ [ "-DBUILD_SHARED_LIBS=OFF" ];
  })).override { mtxclient = final.mtxclient; };

  nix = inputs.nix.packages.${system}.default.overrideAttrs (oa: {
    doInstallCheck = false;
    patches = [ ./profiles/nix/nix.patch ./profiles/nix/expr-context.patch ] ++ oa.patches or [ ];
  });

  mako = prev.mako.overrideAttrs (_: {
    postInstall =
      "sed 's|Exec=.*|Exec=/run/current-system/sw/bin/systemctl --user start mako|' -i $out/share/dbus-1/services/fr.emersion.mako.service";
  });

  codebraid = prev.codebraid.overrideAttrs (_: {
    src = inputs.codebraid;
  });
}
