{ pkgs, lib }: { device, key, initRdOptions }:

let
  mkSurround = pkgs.callPackage ./mkSurround.nix {};
  keyMountPoint = "/key";
in

initRdOptions // {
  kernelModules = initRdOptions.kernelModules ++ [
    key.device.fsType
    "usb_storage"
    "loop"
  ];

  preLVMCommands = mkSurround ''
    mkdir -m 0755 -p ${keyMountPoint}
    echo -n "Waiting for key device to appear.."
    while [ ! -e "${key.device.device}" ]
    do
      echo -n "."
      sleep 0.25
    done
    echo -n " done!"
    echo
    mount -n -t ${key.device.fsType} -o ro "${key.device.device}" ${keyMountPoint}
  '' ''
    echo "Closing key device..."
    umount ${keyMountPoint}
    rmdir ${keyMountPoint}
  '';

  luks.devices.crypt = {
    device = device;
    keyFile = keyMountPoint + key.keyPath;
    header = keyMountPoint + key.headerPath;
    preLVM = true;
  };
}
