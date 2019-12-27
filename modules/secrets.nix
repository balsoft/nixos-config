{ pkgs, config, lib, ... }:
with lib;
with types;
let
  secret = description:
  mkOption {
    inherit description;
    type = nullOr string;
  };
  mkCredOption = service: extra:
  mkOption {
    description = "Credentials for ${service}";
    type = nullOr (submodule {
      options = {
        user = mkOption {
          type = string;
          description = "Username for ${service}";
        };
        password = mkOption {
          type = string;
          description = "Password for ${service}";
        };
      } // extra;
    });
  };
in rec {
  options.secrets = {
    owm-key = secret "OpenWeatherMap key";
    irc = mkCredOption "IRC (konversation)" { };
    slack-term = mkOption { type = string; };
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
      email = mkOption { type = lib.types.string; };
      client-id = mkOption { type = lib.types.string; };
      client-secret = mkOption { type = lib.types.string; };
      refresh-token = mkOption { type = lib.types.string; };
    };
    mail = mkCredOption "email" {
      host = mkOption {
        type = string;
        description = "Mail server";
      };
    };
    gpmusic = mkCredOption "Google Play Music (mopidy)" {
      deviceid = mkOption {
        type = string;
        description = "Android device ID";
      };
    };
    openvpn = mkCredOption "openvpn" {};
    rclone = mkOption {
      type = nullOr string;
      description = "Rclone config";
    };
    id_rsa = mkOption {
      type = nullOr string;
      description = "SSH RSA private key";
    };
    ssl = rec {
      cert = mkOption {
        type = nullOr string;
        description = "SSL certificate";
      };
      priv = mkOption {
        type = nullOr string;
        description = "SSL RSA private key";
      };
    };
    matrix = mkCredOption "matrix" rec {
      shared_secret = mkOption {
        type = nullOr string;
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
    secretnix = import ../secret.nix;
    secrets = if isNull secretnix then
      mapAttrs (n: v: null) options.secrets
    else
      secretnix;
  in { inherit secrets; };
}
