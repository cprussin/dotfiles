{ callPackage, writeShellScript, config }:
let
  mkTerminalApp = callPackage ./mkTerminalApp.nix { inherit config; };
in
name: cmd: mkTerminalApp name "modal" (
  writeShellScript "${name}-modal" cmd
)
