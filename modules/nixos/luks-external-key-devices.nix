{ config, lib, ... }:

let
  cfg = config.boot.luks-external-key-devices;

  keyFsTypes = devices:
    lib.unique (
      lib.mapAttrsToList (
        _: deviceOptions:
          deviceOptions.keyFilesystem.fsType
      ) devices
    );

  awaitingMsg = devices:
    let
      plural = if builtins.length (builtins.attrNames devices) == 1
      then ""
      else "s";
    in
      "Waiting for key filesystem${plural} to appear..";

  awaitCondition = devices:
    lib.concatStringsSep " -o " (
      lib.mapAttrsToList (
        _: deviceOptions:
          "! -e ${deviceOptions.keyFilesystem.device}"
      ) devices
    );
in

{
  options.boot.luks-external-key-devices = lib.mkOption {
    description = ''
      An attrset containing descriptions of luks devices to unlock using key
      materials stored on external filesystems.  The key filesystems are mounted
      temporarily for unlocking the luks device.
    '';
    default = null;
    type = lib.types.nullOr (
      lib.types.attrsOf (
        lib.types.submodule {
          options = {
            device = lib.mkOption {
              type = lib.types.str;
              description = "The path to the block device storing the root filesystem.";
            };

            keyFilesystem = lib.mkOption {
              description = ''
                The filesystem containing the keyfile and luks header.
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
        }
      )
    );
  };

  config = lib.mkIf (cfg != null) {
    boot.initrd = {
      kernelModules = [ "usb_storage" "loop" ] ++ (keyFsTypes cfg);

      preLVMCommands = lib.mkMerge [
        (
          lib.mkBefore (
            lib.concatStringsSep "\n" (
              (
                lib.mapAttrsToList (
                  name: _:
                    "mkdir -m 0755 -p /${name}-key"
                ) cfg
              ) ++ [
                ''
                  echo -n "${awaitingMsg cfg}"
                  while [ ${awaitCondition cfg} ]
                  do
                    echo -n "."
                    sleep 0.25
                  done
                  echo -n " done!"
                  echo
                ''
              ] ++ (
                lib.mapAttrsToList (
                  name: deviceOptions:
                    ''
                      mount -n \
                        -t ${deviceOptions.keyFilesystem.fsType} \
                        -o ro \
                        "${deviceOptions.keyFilesystem.device}" /${name}-key
                    ''
                ) cfg
              )
            )
          )
        )

        (
          lib.mkAfter (
            lib.concatStringsSep "\n" (
              [ "echo 'Closing key device...'" ] ++ (
                lib.mapAttrsToList (
                  name: _:
                    ''
                      umount /${name}-key
                      rmdir /${name}-key
                    ''
                ) cfg
              )
            )
          )
        )
      ];

      luks.devices = lib.mapAttrs (
        name: deviceOptions:
          {
            device = deviceOptions.device;
            keyFile = "/${name}-key" + deviceOptions.keyPath;
            header = "/${name}-key" + deviceOptions.headerPath;
            preLVM = true;
          }
      ) cfg;
    };
  };
}
