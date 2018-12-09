{ lib, ... }:
{
    genIni = lib.generators.toINI {
        mkKeyValue = key: value:
            let
            mvalue =
                if builtins.isBool value then (if value then "true" else "false")
                else if (builtins.isString value && key != "include-file") then value
                else builtins.toString value;
            in
            "${key}=${mvalue}";
    };
    
}
