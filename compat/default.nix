{ ... }: (builtins.getFlake (toString ../.)).legacyPackages.${builtins.currentSystem}
