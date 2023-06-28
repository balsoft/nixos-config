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

  pass-secret-service = (prev.pass-secret-service.override {
    python3 = final.python3 // { pkgs = final.python3Packages; };
  }).overrideAttrs (_: {

    src = final.fetchFromGitHub {
      owner = "mdellweg";
      repo = "pass_secret_service";
      rev = "fadc09be718ae1e507eeb8719f3a2ea23edb6d7a";
      hash = "sha256-lrNU5bkG4/fMu5rDywfiI8vNHyBsMf/fiWIeEHug03c=";
    };
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
    inputs.nix-direnv.packages.${system}.default.override { nix = final.nix; };

  # For nix-direnv
  nixFlakes = final.nix;

  coeurl = final.stdenv.mkDerivation {
    name = "coeurl";
    src = inputs.coeurl;
    buildInputs = [ final.curl.all final.libevent final.spdlog ];
    nativeBuildInputs =
      [ final.meson final.ninja final.pkg-config final.cmake ];
  };

  # mtxclient = prev.mtxclient.overrideAttrs (oa: {
  #   src = inputs.mtxclient;
  #   cmakeFlags = oa.cmakeFlags ++ [ "-DCMAKE_CXX_FLAGS=-DSPDLOG_FMT_EXTERNAL" ];
  #   buildInputs = oa.buildInputs ++ [
  #     final.libevent
  #     final.curl.all
  #     final.coeurl
  #     final.spdlog.dev
  #     final.re2
  #   ];
  #   patches = [ ];
  # });

  # nheko = final.qt6.callPackage (prev.nheko.overrideAttrs (oa: {
  #   src = inputs.nheko;
  #   postPatch = ''
  #     substituteInPlace CMakeLists.txt --replace "# Fixup bundled keychain include dirs" "find_package(Boost COMPONENTS iostreams system  thread REQUIRED)"
  #   '';
  #   buildInputs = oa.buildInputs ++ [
  #     final.xorg.libXdmcp
  #     final.pcre
  #     final.libunwind
  #     final.elfutils
  #     final.coeurl
  #     final.curl
  #     final.libevent
  #     final.asciidoc
  #     final.re2
  #   ];
  #   cmakeFlags = oa.cmakeFlags ++ [ "-DBUILD_SHARED_LIBS=OFF" ];
  # })).override { mtxclient = final.mtxclient; };

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

    audiotube = prev'.audiotube.overrideAttrs (oa: {
      desktopItem = final.makeDesktopItem {
        name = "audiotube";
        desktopName = "Audiotube (proxy)";
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

    pypass = prev'.pypass.overrideAttrs (o:
      let
        version = "f86cf0ba0e5cb6a1236ff16d8f238b92bc49c517";
        sha256 = "sha256-PEPgWdsBjyHpgqPx2MNtYnn0wxI0KtlE+uCD7xO0pvE=";
      in {
        inherit version;

        src = final.fetchFromGitHub {
          owner = "nazarewk";
          # see https://github.com/aviau/python-pass/pull/34
          repo = "python-pass";
          rev = version;
          inherit sha256;
        };
        doInstallCheck = false;
        # Set absolute nix store paths to the executables that pypass uses
        patches = [
          (with final;
            substituteAll {
              src = final.fetchpatch {
                url =
                  "https://raw.githubusercontent.com/nazarewk-iac/nix-configs/4eb0baf5e5b3692c07e626316257c115b7c79b3a/packages/overlays/pypass-mark-executables.patch";
                hash = "sha256-V8HIeaK+EYX8bgodFumki10xynNqS3u6RCkHnsyxTCg=";
              };
              VERSION = version;
              git_exec = "${git}/bin/git";
              grep_exec = "${gnugrep}/bin/grep";
              gpg_exec = "${gnupg}/bin/gpg2";
              tree_exec = "${tree}/bin/tree";
              xclip_exec = "${xclip}/bin/xclip";
            })
        ];
        postPatch = ''
          sed -i 's/@VERSION@/0.2.1/' setup.py
          sed -i 's/enum34//' requirements.txt
        '';
      });
  });

}
