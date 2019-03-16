{ rustPlatform, pkgconfig, libudev }:
rustPlatform.buildRustPackage 
{
  name = "kanshi";
  src = builtins.fetchGit
  {
    url = "https://github.com/emersion/kanshi";
    rev = "970267e400c21a6bb51a1c80a0aadfd1e6660a7b";
  };
  
  buildInputs = [ pkgconfig libudev ];
  cargoSha256 = "sha256:0lf1zfmq9ypxk86ma0n4nczbklmjs631wdzfx4wd3cvhghyr8nkq";
}
