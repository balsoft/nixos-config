{ buildVscodeMarketplaceExtension }: {
  dhall.vscode-dhall-lsp-server = buildVscodeMarketplaceExtension {
    mktplcRef = {
      publisher = "dhall";
      name = "vscode-dhall-lsp-server";
      version = "0.0.4";
      sha256 = "sha256-WopWzMCtiiLrx3pHNiDMZYFdjS359vu3T+6uI5A+Nv4=";
    };
  };
  gpoore.codebraid-preview = buildVscodeMarketplaceExtension {
    mktplcRef = {
      publisher = "gpoore";
      name = "codebraid-preview";
      version = "0.8.0";
      sha256 = "sha256-bw/uzdbLDREwhd0PHOD+Ref3UdaYlkhegsUhkTX1WlI=";
    };
  };
}
