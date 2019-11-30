{ config, lib, ... }:

let
  cfg = config.boot.crypt-initrd;
  keyMountPoint = "/key";
in

{
  options.boot.crypt-initrd = {
    enable = lib.mkEnableOption "initrd to boot encrypted root";

    device = lib.mkOption {
      type = lib.types.str;
      description = "The path to the block device storing the root filesystem.";
    };

    key = lib.mkOption {
      type = lib.types.submodule {
        options = {
          device = lib.mkOption {
            description = ''
              The key device containing the keyfile and luks header.
            '';

            type = lib.types.submodule {
              options = {
                device = lib.mkOption {
                  type = lib.types.str;
                  description = "The path to the block device node.";
                };

                fsType = lib.mkOption {
                  type = lib.types.str;
                  description = "The type of filesystem used for the device.";
                };
              };
            };
          };

          keyPath = lib.mkOption {
            type = lib.types.str;
            description = ''
              The path to the key file relative to the root of the key device.
            '';
          };

          headerPath = lib.mkOption {
            type = lib.types.str;
            description = ''
              The path to the luks header relative to the root of the key
              device.
            '';
          };
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    boot.initrd = {
      kernelModules = [ cfg.key.device.fsType "usb_storage" "loop" ];

      preLVMCommands = lib.mkMerge [
        (
          lib.mkBefore ''
            mkdir -m 0755 -p ${keyMountPoint}
            echo -n "Waiting for key device to appear.."
            while [ ! -e "${cfg.key.device.device}" ]
            do
              echo -n "."
              sleep 0.25
            done
            echo -n " done!"
            echo
            mount -n \
              -t ${cfg.key.device.fsType} \
              -o ro \
              "${cfg.key.device.device}" ${keyMountPoint}
          ''
        )
        (
          lib.mkAfter ''
            echo "Closing key device..."
            umount ${keyMountPoint}
            rmdir ${keyMountPoint}
          ''
        )
      ];

      luks.devices.crypt = {
        device = cfg.device;
        keyFile = keyMountPoint + cfg.key.keyPath;
        header = keyMountPoint + cfg.key.headerPath;
        preLVM = true;
      };
    };
  };
}
