{
  inputs,
  pkgs,
  config,
  lib,
  ...
}:
{
  secrets."FNMT_certificado.p12" = {
    owner = "balsoft";
    services = [ ];
  };

  home-manager.users.balsoft = {
    imports = [ inputs.autofirma-nix.homeManagerModules.default ];

    programs.firefox.package = pkgs.librewolf;

    # Enable AutoFirma with Firefox integration
    programs.autofirma = {
      enable = true;

      config = {
        defaultKeystore = "PKCS#12 / PFX";
        useDefaultStoreInBrowserCalls = true;
        defaultLocalKeystorePath = config.secrets."FNMT_certificado.p12".decrypted;
      };
    };

    programs.librewolf = {
      policies = {
        SecurityDevices = {
          "OpenSC PKCS11" = "${pkgs.opensc}/lib/opensc-pkcs11.so";
        };
        Certificates = {
          ImportEnterpriseRoots = true;
          Install = [
            "${config.home-manager.users.balsoft.home.homeDirectory}/.afirma/Autofirma/Autofirma_ROOT.cer"
          ];
        };
      };

      profiles.default.settings = {
        "network.protocol-handler.app.afirma" =
          "${config.home-manager.users.balsoft.programs.autofirma.finalPackage}/bin/autofirma";
        "network.protocol-handler.warn-external.afirma" = false;
        "network.protocol-handler.external.afirma" = true;
      };
    };
  };
}
