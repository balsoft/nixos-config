{
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      X11Forwarding = true;
    };
    extraConfig = "StreamLocalBindUnlink yes";
    ports = [ 22 ];
  };

  persist.state.directories = [
    "/etc/ssh"
  ];

  persist.state.homeDirectories = [ ".ssh" ];

  users.users.balsoft.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDd2OdcSHUsgezuV+cpFqk9+Svtup6PxIolv1zokVZdqvS8qxLsA/rwYmQgTnuq4/zK/GIxcUCH4OxYlW6Or4M4G7qrDKcLAUrRPWkectqEooWRflZXkfHduMJhzeOAsBdMfYZQ9024GwKr/4yriw2BGa8GbbAnQxiSeTipzvXHoXuRME+/2GsMFAfHFvxzXRG7dNOiLtLaXEjUPUTcw/fffKy55kHtWxMkEvvcdyR53/24fmO3kLVpEuoI+Mp1XFtX3DvRM9ulgfwZUn8/CLhwSLwWX4Xf9iuzVi5vJOJtMOktQj/MwGk4tY/NPe+sIk+nAUKSdVf0y9k9JrJT98S/ comment"
  ];

  # secrets.ssh_key = {
  #   services = [ ];
  #   decrypted = "/root/.ssh/id_ed25519";
  # };

  home-manager.users.balsoft.programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
  };
}
