lib: rec {
  mkKeyValue = key: value:
    let
      mvalue = if builtins.isBool value then
        (if value then "true" else "false")
      else if builtins.isString value then
        value
      else
        builtins.toString value;
    in "${key}=${mvalue}";
  attrsToList = with builtins;
    x:
    (map (key: {
      name = key;
      value = getAttr key x;
    }) (attrNames x));

  genIni = lib.generators.toINI { inherit mkKeyValue; };
  genIniOrdered = lst:
    (builtins.concatStringsSep "\n" (map ({ name ? "widget", ... }@attrs:
      builtins.concatStringsSep "\n" ([ "[${name}]" ]
        ++ (map ({ name, value }: mkKeyValue name value)
          (attrsToList (builtins.removeAttrs attrs [ "name" ]))))) lst)) + "\n";
  splitHex = hexStr:
    map (x: builtins.elemAt x 0)
    (builtins.filter (a: a != "" && a != [ ]) (builtins.split "(.{2})" hexStr));
  hex2decDigits = rec {
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
    A = a;
    B = b;
    C = c;
    D = d;
    E = e;
    F = f;
  };

  dec2hexDigits = lib.mapAttrs' (lib.flip lib.nameValuePair)
    (builtins.mapAttrs (_: toString) hex2decDigits);

  doubleDigitHexToDec = hex:
    16 * hex2decDigits."${builtins.substring 0 1 hex}"
    + hex2decDigits."${builtins.substring 1 2 hex}";

  decToHex = n: dec2hexDigits.${toString (builtins.div n 16)} + dec2hexDigits.${toString (builtins.mod n 16)};

  thmDec = builtins.mapAttrs (name: color: colorHex2Dec color);
  thmHash = builtins.mapAttrs (name: color: "#${color}");

  triplet2RGB = lst: {
      r = builtins.elemAt lst 0;
      g = builtins.elemAt lst 1;
      b = builtins.elemAt lst 2;
  };

  hex2RGB = color: triplet2RGB (map doubleDigitHexToDec (splitHex color));
  dec2RGB = color: triplet2RGB (map (builtins.fromJSON) (lib.splitString "," color));

  RGB2hex = { r, g, b }: decToHex r + decToHex g + decToHex b;
  RGB2dec = { r, g, b }: lib.concatMapStringsSep "," toString [ r g b ];

  mulRGB = factor: builtins.mapAttrs (_: x: builtins.floor (x * factor));

  mulHex = factor: color: RGB2hex (mulRGB factor (hex2RGB color));

  mulDec = factor: color: RGB2dec (mulRGB factor (dec2RGB color));

  colorHex2Dec = color:
    builtins.concatStringsSep ","
    (map (x: toString (doubleDigitHexToDec x)) (splitHex color));

}
