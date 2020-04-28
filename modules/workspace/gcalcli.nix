{ pkgs, lib, config, ... }: {
  home-manager.users.balsoft = lib.mkIf (! isNull config.secrets.gcal) {
    home.file.".gcalcli_oauth.home".text = lib.optionals
    (!(isNull config.secrets.gcal)) (builtins.toJSON {
      access_token = "";
      client_id = config.secrets.gcal.client-id;
      client_secret = config.secrets.gcal.client-secret;
      refresh_token = config.secrets.gcal.refresh-token;
      token_expiry = "2019-03-31T11:26:27Z";
      token_uri = https://oauth2.googleapis.com/token;
      user_agent = "gcalcli/v4.0.4";
      revoke_uri = "https=//oauth2.googleapis.com/revoke";
      id_token = null;
      id_token_jwt = null;
      token_response = {
        access_token = "";
        expires_in = 0;
        scope =
          "https=//www.googleapis.com/auth/urlshortener https=//www.googleapis.com/auth/calendar";
        token_type = "Bearer";
      };
      scopes = [
        "https=//www.googleapis.com/auth/calendar"
        "https=//www.googleapis.com/auth/urlshortener"
      ];
      token_info_uri = "https=//oauth2.googleapis.com/tokeninfo";
      invalid = false;
      _class = "OAuth2Credentials";
      _module = "oauth2client.client";
    });
    home.activation.gcalcli = {
      after = ["linkGeneration"];
      before = [];
      data = "cp .gcalcli_oauth.home .gcalcli_oauth";
    };
  };
}
