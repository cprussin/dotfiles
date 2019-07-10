{ pkgs, lib }: { device, key, initRdOptions }:

let
  mkSurround = pkgs.callPackage ./mkSurround.nix {};
in

initRdOptions // {
  kernelModules = initRdOptions.kernelModules ++ [
    key.device.fsType
    "usb_storage"
    "nls_cp437"
    "nls_iso8859_1"
    "loop"
  ];

  preLVMCommands = mkSurround ''
    mkdir -m 0755 -p /key
    echo -n "Waiting for key device to appear.."
    while [ ! -e "${key.device.device}" ]
    do
      echo -n "."
      sleep 0.25
    done
    echo -n " done!"
    echo
    mount -n -t ${key.device.fsType} -o ro "${key.device.device}" /key
  '' ''
    echo "Closing key device..."
    umount /key
    rmdir /key
  '';

  luks.devices.crypt = {
    device = device;
    keyFile = "/key${key.keyPath}";
    header = "/key${key.headerPath}";
    preLVM = true;
  };
}
