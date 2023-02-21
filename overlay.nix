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
        python3Packages.ytmusicapi
        python3Packages.pytube

      ];

      doCheck = false;
    };

  inherit (nur.balsoft.pkgs) termNote nix-patch;

  lambda-launcher = inputs.lambda-launcher.defaultPackage.${system};

  simple-osd = inputs.simple-osd-daemons.packages.${system};

  nerdfonts = nur.balsoft.pkgs.roboto-mono-nerd;

  pass-secret-service = prev.pass-secret-service.overrideAttrs (_: {
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

  nix-direnv =
    inputs.nix-direnv.packages.${system}.default.override { pkgs = final; };

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
    buildInputs = oa.buildInputs ++ [
      final.libevent
      final.curl.all
      final.coeurl
      final.spdlog.dev
      final.re2
    ];
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

  nix = inputs.nix.packages.${system}.default;

  nil = prev.nil.overrideAttrs (_: {
    doCheck = false;
    doInstallCheck = false;
  });

  mako = prev.mako.overrideAttrs (_: {
    postInstall =
      "sed 's|Exec=.*|Exec=/run/current-system/sw/bin/systemctl --user start mako|' -i $out/share/dbus-1/services/fr.emersion.mako.service";
  });

  codebraid = prev.codebraid.overrideAttrs (_: { src = inputs.codebraid; });

  remapper = inputs.remapper.packages.${final.system}.default;

  helix = inputs.helix.packages.${final.system}.default;

  plasma5Packages = prev.plasma5Packages.overrideScope' (final': prev': {
    qmltermwidget = prev'.qmltermwidget.overrideAttrs (_: {
      src = final.fetchFromGitHub {
        owner = "balsoft";
        repo = "qmltermwidget";
        rev = "39911727b7ce70a100aad84c1cdae9ab0ead1d6b";
        hash = "sha256-Y+e9WjkXl38tbJq5D2BH0wJ0157yBiQ3vfRYO6rwnoU=";
      };
      patches = [ ];
    });

    qmlkonsole = final'.callPackage ({ lib, mkDerivation, cmake
      , extra-cmake-modules, kconfig, ki18n, kirigami-addons, kirigami2
      , kcoreaddons, qtquickcontrols2, kwindowsystem, qmltermwidget }:

      mkDerivation {
        pname = "qmlkonsole";

        inherit ((final.callPackage
          "${inputs.nixpkgs}/pkgs/applications/plasma-mobile/srcs.nix" {
            mirror = "mirror://kde";
          }).qmlkonsole)
          version src;

        nativeBuildInputs = [ cmake extra-cmake-modules ];

        buildInputs = [
          kconfig
          ki18n
          kirigami-addons
          kirigami2
          qtquickcontrols2
          kcoreaddons
          kwindowsystem
          qmltermwidget
        ];

        meta = with lib; {
          description = "Terminal app for Plasma Mobile";
          homepage = "https://invent.kde.org/plasma-mobile/qmlkonsole";
          license = with licenses; [ gpl2Plus gpl3Plus cc0 ];
          maintainers = with maintainers; [ balsoft ];
        };
      }) { };

    audiotube = prev'.audiotube.overrideAttrs (_: {
      desktopItem = final.makeDesktopItem {
        name = "Audiotube";
        exec = "https_proxy=socks5://localhost:5555 audiotube";
        icon = "org.kde.audiotube";
        type = "Application";
        categories = [ "Qt" "KDE" "AudioVideo" "Player" ];
      };
    });

  });
  okularMobile = final.okular.overrideAttrs
    (oa: { cmakeFlags = oa.cmakeFlags or [ ] ++ [ "-DOKULAR_UI=mobile" ]; });

  python3Packages = prev.python3Packages.overrideScope (final': prev': {
    yt-dlp = prev'.yt-dlp.overrideAttrs (_: { src = inputs.yt-dlp; });
  });

}
