{ lib, config, ... }:
let
  mkKeyValue = key: value:
    let
      mvalue = if builtins.isBool value then
        (if value then "true" else "false")
      else if (builtins.isString value && key != "include-file") then
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
in rec {
  genIni = lib.generators.toINI { inherit mkKeyValue; };
  genIniOrdered = lst:
    (builtins.concatStringsSep "\n" (map ({ name ? "widget", ... }@attrs:
      builtins.concatStringsSep "\n" ([ "[${name}]" ]
        ++ (map ({ name, value }: mkKeyValue name value)
          (attrsToList (builtins.removeAttrs attrs [ "name" ]))))) lst)) + "\n";
  thm = config.themes.colors;
  splitHex = hexStr:
    map (x: builtins.elemAt x 0) (builtins.filter (a: a != "" && a != [ ])
      (builtins.split "(.{2})" (builtins.substring 1 6 hexStr)));
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

  doubleDigitHexToDec = hex:
    16 * hex2decDigits."${builtins.substring 0 1 hex}"
    + hex2decDigits."${builtins.substring 1 2 hex}";
  thmDec = builtins.mapAttrs (name: color: colorHex2Dec color) thm;
  colorHex2Dec = color:
    builtins.concatStringsSep ","
    (map (x: toString (doubleDigitHexToDec x)) (splitHex color));

}
