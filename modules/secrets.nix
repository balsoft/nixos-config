{ pkgs, config, lib, inputs, ... }:
with lib;
with types;
let
  secret = description:
  mkOption {
    inherit description;
    type = nullOr str;
  };
  mkCredOption = service: extra:
  mkOption {
    description = "Credentials for ${service}";
    type = nullOr (submodule {
      options = {
        user = mkOption {
          type = str;
          description = "Username for ${service}";
        };
        password = mkOption {
          type = str;
          description = "Password for ${service}";
        };
      } // extra;
    });
  };
in rec {
  options.secrets = {
    owm-key = secret "OpenWeatherMap key";
    irc = mkCredOption "IRC (konversation)" { };
    slack-term = mkOption { type = str; };
    yt-utilities = {
      user = secret "youtrack user";
      url = secret "youtrack url";
      token =  secret "youtrack token";
      source = {
        url = secret "A url to yt-utilities source";
        rev = secret "revision";
        sha256 = secret "sha256";
      };
    };
    wage = secret "wage (sum CURRENCY/TIME, like 10EUR/h)";
    gcal = {
      email = mkOption { type = lib.types.str; };
      client-id = mkOption { type = lib.types.str; };
      client-secret = mkOption { type = lib.types.str; };
      refresh-token = mkOption { type = lib.types.str; };
    };
    mail = mkCredOption "email" {
      host = mkOption {
        type = str;
        description = "Mail server";
      };
    };
    gpmusic = mkCredOption "Google Play Music (mopidy)" {
      deviceid = mkOption {
        type = str;
        description = "Android device ID";
      };
    };
    openvpn = mkCredOption "openvpn" {};
    rclone = mkOption {
      type = nullOr str;
      description = "Rclone config";
    };
    ssl = rec {
      cert = mkOption {
        type = nullOr str;
        description = "SSL certificate";
      };
      priv = mkOption {
        type = nullOr str;
        description = "SSL RSA private key";
      };
    };
    matrix = mkCredOption "matrix" rec {
      shared_secret = mkOption {
        type = nullOr str;
        description = "A shared secret for matrix instance";
      };
      mautrix-whatsapp = {
        config = mkOption {
          type = attrs;
        };
        registration = mkOption {
          type = attrs;
        };
      };
      mautrix-telegram = mautrix-whatsapp;
    };
  };
  config = let
    unlocked = import (pkgs.runCommand "check-secret" {} "set +e; grep -qI . ${../secret.nix}; echo $? > $out") == 0;
    secretnix = import ../secret.nix;
    secrets = if ! unlocked || isNull secretnix then
      mapAttrs (n: v: null) options.secrets
    else
      secretnix;
  in { inherit secrets; };
}
