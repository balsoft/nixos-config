{ pkgs, config, lib, ... }: {
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };

  users.users.balsoft.openssh.authorizedKeys.keys =
  ["ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDd2OdcSHUsgezuV+cpFqk9+Svtup6PxIolv1zokVZdqvS8qxLsA/rwYmQgTnuq4/zK/GIxcUCH4OxYlW6Or4M4G7qrDKcLAUrRPWkectqEooWRflZXkfHduMJhzeOAsBdMfYZQ9024GwKr/4yriw2BGa8GbbAnQxiSeTipzvXHoXuRME+/2GsMFAfHFvxzXRG7dNOiLtLaXEjUPUTcw/fffKy55kHtWxMkEvvcdyR53/24fmO3kLVpEuoI+Mp1XFtX3DvRM9ulgfwZUn8/CLhwSLwWX4Xf9iuzVi5vJOJtMOktQj/MwGk4tY/NPe+sIk+nAUKSdVf0y9k9JrJT98S/ comment"];
  home-manager.users.balsoft.programs.ssh =
  if (!isNull config.secrets.id_rsa) then {
    enable = true;
    matchBlocks = {
      "*" = {
        identityFile = toString (pkgs.writeTextFile {
          name = "id_rsa";
          text = config.secrets.id_rsa;
        });
        compression = false;
      };
    };
  } else
    { };
}
