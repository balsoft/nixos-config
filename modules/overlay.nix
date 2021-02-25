{ pkgs, config, lib, inputs, ... }:
let
  filterGit =
    builtins.filterSource (type: name: name != ".git" || type != "directory");
  system = "x86_64-linux";
  old = import inputs.nixpkgs-old ({
    config = config.nixpkgs.config;
    localSystem = { inherit system; };
  });
in {
  nixpkgs.overlays = [
    # inputs.nix.overlay
    (self: super: rec {
      nix = super.nix // {
        meta = super.nix.meta // { platforms = lib.platforms.unix; };
      };

      nur = (import inputs.NUR {
        pkgs = old;
        nurpkgs = pkgs;
      }).repos;

      inherit (nur.balsoft.pkgs) termNote nix-patch;

      # inherit (old) mautrix-telegram;

      lambda-launcher = inputs.lambda-launcher.defaultPackage.x86_64-linux;

      simple-osd = inputs.simple-osd-daemons.packages.x86_64-linux;

      inherit old;

      yt-utilities = inputs.yt-utilities.defaultPackage.x86_64-linux;

      # mopidy-youtube = super.mopidy-youtube.overrideAttrs (_: { src = inputs.mopidy-youtube; });

      nerdfonts = nur.balsoft.pkgs.roboto-mono-nerd;

      weechatScripts.wee-slack = super.weechatScripts.wee-slack.overrideAttrs (oa: {
        src = inputs.wee-slack;
        patches = [(builtins.elemAt oa.patches 0)];
      });

      # inherit (inputs.nixpkgs-mesa.legacyPackages.x86_64-linux) sway mesa_drivers;
    })
  ];
  nixpkgs.config = {
    allowUnfree = true;
    android_sdk.accept_license = true;
  };
}
