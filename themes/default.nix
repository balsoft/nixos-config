{ pkgs, thm, genIni }:
let
  splitHex = hexStr: map (x: builtins.elemAt x 0) (builtins.filter (a: a != "" && a != []) (builtins.split "(.{2})" (builtins.substring 1 6 hexStr)));
  hex2decDigits = {
		"0" = 0;	
		"1" = 1;
		"2" = 2;
		"3" = 3;
		"4" = 4;
		"5" = 5;
		"6" = 6;
		"7" = 7;
		"8" = 8;
		"9" = 9;
		"a" = 10;
		"b" = 11;
		"c" = 12;
		"d" = 13;
		"e" = 14;
		"f" = 15;
	};

	doubleDigitHexToDec = hex: 16 * hex2decDigits."${builtins.substring 0 1 hex}" + hex2decDigits."${builtins.substring 1 2 hex}";
	thmDec = builtins.mapAttrs (name: color: colorHex2Dec color) thm;
  thm' = builtins.mapAttrs (name: value: builtins.substring 1 7 value) thm;
  colorHex2Dec = color: builtins.concatStringsSep "," (map (x: toString (doubleDigitHexToDec x)) (splitHex color));
in
rec {
  materia_colors = pkgs.writeTextFile {
    name = "gtk-generated-colors";
    text = ''
    BG=${thm'.bg}
    FG=${thm'.fg}
    BTN_BG=${thm'.dark}
    BTN_FG=${thm'.fg}
    MENU_BG=${thm'.bg}
    MENU_FG=${thm'.fg}
    ACCENT_BG=${thm'.cyan}
    SEL_BG=${thm'.cyan}
    SEL_FG=${thm'.bg}
    TXT_BG=${thm'.bg}
    TXT_FG=${thm'.fg}
    HDR_BTN_BG=${thm'.bg}
    HDR_BTN_FG=${thm'.fg}
    WM_BORDER_FOCUS=${thm'.blue}
    WM_BORDER_UNFOCUS=${thm'.alt}
    MATERIA_STYLE_COMPACT=True
    MATERIA_COLOR_VARIANT=dark
    UNITY_DEFAULT_LAUNCHER_STYLE=False
    NAME=nord
    '';
  };
  gtk = pkgs.stdenv.mkDerivation rec {
    name = "generated-gtk-theme";
    src = builtins.fetchGit {
      url = "https://github.com/nana-4/materia-theme";
      rev = "5e11d2aa6cc26f4f7fd8c229214c4e74b802d6b8";
    };
    buildInputs = with pkgs; [ sassc bc which inkscape optipng ];
    installPhase = ''
      cp -r $src $out
      cd $out
      chmod 777 -R .
      patchShebangs .
      substituteInPlace change_color.sh --replace "\$HOME/.themes" "$out"
      echo "Changing colours:"
      ./change_color.sh -o generated ${materia_colors}
      chmod 555 -R .
    '';
  };

  konsole = genIni {
    General = {
      Description = "generated";
      Opacity = 1;
      Wallpaper = "";
    };
    Background.Color = thmDec.bg;
    BackgroundIntense.Color = thmDec.bg;
    Foreground.Color = thmDec.fg;
    Color0.Color = thmDec.dark;
    Color0Intense.Color = thmDec.alt;
    Color1.Color = thmDec.red;
    Color2.Color = thmDec.green;
    Color3.Color = thmDec.yellow;
    Color4.Color = thmDec.blue;
    Color5.Color = thmDec.purple;
    Color6.Color = thmDec.cyan;
    Color7.Color = thmDec.fg;
  };

  kate = genIni {
	  "Breeze Dark"."Color Background" = thmDec.bg;
	};

  albert = genIni {
	  BoxModel = {
		  animation_duration = 0;
			background_color = thm.bg;
			border_color = thm.blue;
			border_size = 1;
			icon_size = 46;
			input_fontsize = 28;
			item_description_fontsize = 20;
			item_title_fontsize = 24;
			max_items = 10;
			padding = 6;
			radius = 2;
			settingsbutton_size = 10;
			spacing = 5;
			window_width = 1200;
		};
	};

  kde = genIni {
  	"Colors:Button" = {
	     BackgroundAlternate = thmDec.dark;
	     BackgroundNormal = thmDec.bg;
	     DecorationFocus = thmDec.blue;
	     DecorationHover = thmDec.blue;
	     ForegroundActive = thmDec.blue;
	     ForegroundInactive = thmDec.alt;
	     ForegroundLink = thmDec.blue;
	     ForegroundNegative = thmDec.red;
	     ForegroundNeutral = thmDec.orange;
	     ForegroundNormal = thmDec.fg;
	     ForegroundPositive = thmDec.green;
	     ForegroundVisited = thmDec.gray;
	  };
	  "Colors:Complementary" = {
	    BackgroundAlternate = thmDec.dark;
	    BackgroundNormal = thmDec.bg;
	    DecorationFocus = thmDec.blue;
	    DecorationHover = thmDec.blue;
	    ForegroundActive = thmDec.orange;
	    ForegroundInactive = thmDec.alt;
	    ForegroundLink = thmDec.blue;
	    ForegroundNegative = thmDec.red;
	    ForegroundNeutral = thmDec.yellow;
	    ForegroundNormal = thmDec.fg;
	    ForegroundPositive = thmDec.green;
	    ForegroundVisited = thmDec.blue;
	  };
	  "Colors:Selection" = {
	    BackgroundAlternate = thmDec.blue;
	    BackgroundNormal = thmDec.blue;
	    DecorationFocus = thmDec.blue;
	    DecorationHover = thmDec.blue;
	    ForegroundActive = thmDec.fg;
	    ForegroundInactive = thmDec.fg;
	    ForegroundLink = thmDec.blue;
	    ForegroundNegative = thmDec.red;
	    ForegroundNeutral = thmDec.orange;
	    ForegroundNormal = thmDec.fg;
	    ForegroundPositive = thmDec.green;
	    ForegroundVisited = thmDec.alt;
	  };
	  "Colors:Tooltip" = {
	    BackgroundAlternate = thmDec.dark;
	    BackgroundNormal = thmDec.bg;
	    DecorationFocus = thmDec.blue;
	    DecorationHover = thmDec.blue;
	    ForegroundActive = thmDec.blue;
	    ForegroundInactive = thmDec.alt;
	    ForegroundLink = thmDec.blue;
	    ForegroundNegative = thmDec.red;
	    ForegroundNeutral = thmDec.orange;
	    ForegroundNormal = thmDec.fg;
	    ForegroundPositive = thmDec.green;
	    ForegroundVisited = thmDec.gray;
	  };
	  "Colors:View" = {
	    BackgroundAlternate = thmDec.dark;
	    BackgroundNormal = thmDec.bg;
	    DecorationFocus = thmDec.blue;
	    DecorationHover = thmDec.blue;
	    ForegroundActive = thmDec.blue;
	    ForegroundInactive = thmDec.alt;
	    ForegroundLink = thmDec.blue;
	    ForegroundNegative = thmDec.red;
	    ForegroundNeutral = thmDec.orange;
	    ForegroundNormal = thmDec.fg;
	    ForegroundPositive = thmDec.green;
	    ForegroundVisited = thmDec.gray;
	  };
	  "Colors:Window" = {
	    BackgroundAlternate = thmDec.dark;
	    BackgroundNormal = thmDec.bg;
	    DecorationFocus = thmDec.blue;
	    DecorationHover = thmDec.blue;
	    ForegroundActive = thmDec.blue;
	    ForegroundInactive = thmDec.alt;
	    ForegroundLink = thmDec.blue;
	    ForegroundNegative = thmDec.red;
	    ForegroundNeutral = thmDec.orange;
	    ForegroundNormal = thmDec.fg;
	    ForegroundPositive = thmDec.green;
	    ForegroundVisited = thmDec.gray;
	  };
	  General = {
	    ColorScheme="Generated";
	  	Name="Generated";
			fixed = "Roboto Mono,11,-1,5,50,0,0,0,0,0";
			font = "Roboto,11,-1,5,50,0,0,0,0,0";
			menuFont = "Roboto,11,-1,5,50,0,0,0,0,0";
			shadeSortColumn = true;
			smallestReadableFont = "Roboto,8,-1,5,57,0,0,0,0,0,Medium";
			toolBarFont = "Roboto,11,-1,5,50,0,0,0,0,0";
	  };
	  KDE = {
	    DoubleClickInterval = 400;  
	    ShowDeleteCommand = true;
	    SingleClick = false;
	    StartDragDist = 4;
	    StartDragTime = 500;
	    WheelScrollLines = 3;
	    contrast = 4;
	    widgetStyle = "Breeze";
	  };
	  Icons = {
    	  Theme="Papirus-Dark";
	  };
	};
}
