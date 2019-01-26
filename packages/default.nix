{ callPackage }:
{
  mconnect = callPackage ./mconnect.nix {};
  vk = callPackage ./vk-messenger.nix {};
}
