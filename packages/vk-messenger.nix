{ stdenv, makeDesktopItem, unzip, steam-run, bash }:
stdenv.mkDerivation rec {
	name = "vk-messenger";
	src = builtins.fetchurl {
		url = "https://desktop.userapi.com/linux64/master/vk.zip";
		sha256 = "8e6f531e568b557657cf90fd09be39be928539d7e4d8bff0038b99fb53feb656";
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
		exec = "vk";
		icon = "vk";
		comment = "Desktop messaging app by Vkontakte team";
		desktopName = "VK Messenger";
		genericName = "Messenger";
		categories = "Messaging;Chat;";
	};

	buildInputs = [ unzip ];
}