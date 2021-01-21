{
  services.vsftpd = {
    enable = true;
    anonymousUser = true;
    allowWriteableChroot = true;
    anonymousMkdirEnable = true;
    anonymousUploadEnable = true;
    writeEnable = true;
  };
}
