{ lib, pkgs }:

let
  passEntry = builtins.extraBuiltins.password pkgs;
  passEntrySplit = name: lib.splitString "\n" (passEntry name);
in

{
  get-password = name: builtins.elemAt (passEntrySplit name) 0;
  get-password-field = name: field:
    lib.replaceStrings [ "${field}: " ] [ "" ] (
      lib.findFirst (lib.hasPrefix "${field}: ") "" (passEntrySplit name)
    );
  get-full-password = passEntry;
}
