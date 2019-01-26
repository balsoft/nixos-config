{ stdenv, makeDesktopItem, unzip, steam-run, bash }:
stdenv.mkDerivation rec {
  name = "vk-messenger";
  src = builtins.fetchurl {
    url = "https://desktop.userapi.com/linux64/master/vk.zip";
    sha256 = "0kk0ibx9hc0hfnv6vgc9pylxdwvsdllxlgp9zc2b2398dv6ddymg";
  };
  unpackPhase = "unzip $src -d $out";
  installPhase = ''
mkdir $out/bin
cat << EOF > $out/bin/vk
#!${bash}/bin/bash
${steam-run}/bin/steam-run $out/vk
EOF
chmod +x $out/bin/vk
'';
  desktopItem = makeDesktopItem {
    name = "vk";
    exec = "bin/vk";
    icon = "vk";
    comment = "Desktop messaging app by Vkontakte team";
    desktopName = "VK Messenger";
    genericName = "Messenger";
    categories = "Messaging;Chat;";
  };

  buildInputs = [ unzip ];
}
