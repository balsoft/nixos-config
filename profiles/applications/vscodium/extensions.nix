{ buildVscodeMarketplaceExtension }: {
  dhall.vscode-dhall-lsp-server = buildVscodeMarketplaceExtension {
    mktplcRef = {
      publisher = "dhall";
      name = "vscode-dhall-lsp-server";
      version = "0.0.4";
      sha256 = "sha256-WopWzMCtiiLrx3pHNiDMZYFdjS359vu3T+6uI5A+Nv4=";
    };
  };
  phoityne.phoityne-vscode = buildVscodeMarketplaceExtension {
    mktplcRef = {
      publisher = "phoityne";
      name = "phoityne-vscode";
      version = "0.0.27";
      sha256 = "sha256-b+my38g6CXhSJn0c3zmZYR9AxpNvFn+gFSQ1FpznHnY=";
    };
  };
}
