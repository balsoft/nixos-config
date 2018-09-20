{ pkgs }:
pkgs.stdenv.mkDerivation rec {
	name = "mconnect";
	src = builtins.fetchGit{
		url = "https://github.com/bboozzoo/mconnect";
		ref = "aa050c9733ba48cc875e3e502c94bbcdcc7959f8";
	};
	
	buildInputs = with pkgs; [ wrapGAppsHook vala glib gobjectIntrospection json-glib gnome3.libgee stdenv libnotify gtk3 at-spi2-core at-spi2-core.dev at-spi2-atk meson pkgconfig dbus gnutls ninja glib_networking ];
}