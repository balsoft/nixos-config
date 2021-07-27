{ config, pkgs, lib, ... }: {
  secrets.aws_credentials = {
    owner = "balsoft:users";
    services = [ ];
  };

  systemd.tmpfiles.rules = [ "L+ /root/.aws/credentials - - - - ${config.secrets.aws_credentials.decrypted}" ];

  environment.sessionVariables = {
    AWS_SHARED_CREDENTIALS_FILE = config.secrets.aws_credentials.decrypted;
    AWS_CONFIG_FILE = toString (pkgs.writeText "aws_config"
      (pkgs.my-lib.genIni { default.region = "eu-west-2"; }));
  };
}
