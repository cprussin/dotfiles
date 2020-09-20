{ lib }:

{
  mkZfsFileSystems = lib.mapAttrs' (
    device: options:
      lib.nameValuePair options.mountpoint {
        inherit device;
        neededForBoot = options.neededForBoot or false;
        fsType = "zfs";
      }
  );
}
