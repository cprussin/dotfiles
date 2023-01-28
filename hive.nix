{
  sources ? import ./sources.nix,
  nixpkgs ? sources.nixpkgs,
  machineDir ? ./config/machines,
}: let
  mkMachine = targetHost: {...}: {
    deployment = {inherit targetHost;};
    imports = ["${toString machineDir}/${targetHost}"];
  };

  all-machines =
    builtins.mapAttrs
    (machine: _: mkMachine machine)
    (builtins.readDir machineDir);
in
  {
    meta = {
      nixpkgs = import nixpkgs {};
      name = "PrussinNet";
      description = "PrussinNet services";
    };
  }
  // all-machines
