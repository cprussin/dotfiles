{
  sources ? import ./sources.nix,
  nixpkgs ? sources.nixpkgs,
  machineDir ? ./config/machines,
}: let
  mkMachine = targetHost: {config, ...}: {
    deployment = {
      inherit targetHost;
      targetUser = config.primary-user.name;
    };
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
