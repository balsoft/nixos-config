{ pkgs }:
{
    mconnect = import ./mconnect.nix {inherit pkgs;};
}