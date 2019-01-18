{ pkgs }:
{
  mconnect = pkgs.callPackage ./mconnect.nix {};
  vk = pkgs.callPackage ./vk-messenger.nix {};
}
