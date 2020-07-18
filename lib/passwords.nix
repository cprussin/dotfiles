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
  get-base64-encoded-password = builtins.extraBuiltins.base64Password pkgs;
}
