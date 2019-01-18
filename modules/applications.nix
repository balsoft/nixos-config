{pkgs, config, lib, ...}:
{
  options.defaultApplications = lib.mkOption
  {
    type = lib.types.attrs;
    description = "Preferred applications";
  };
  config = {};
}
