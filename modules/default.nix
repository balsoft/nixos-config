{pkgs, lib, config, ...}:
with lib;
let mergeAndImport = list: { config, ... }: foldl (acc: x: { options= mkMerge [acc.options x.options]; config = mkMerge[acc.config x.config]; }) { options = {}; config = {}; } (map (f: import f {inherit pkgs lib config;}) list);
in
{
  options.home-manager.users = lib.mkOption {
  type = types.attrsOf (types.submodule (mergeAndImport [
    ./themes.nix
  ]));
  };
}
