{ callPackage, writeShellScript, terminal }:

let
  mkTerminalApp = callPackage ./mkTerminalApp.nix { inherit terminal; };
in

name: cmd: mkTerminalApp name "modal" (
  writeShellScript "${name}-modal" cmd
)
