{
  config,
  lib,
  pkgs,
  utils,
  ...
}: let
  cfg = config.boot.initrd.luksWithKeyDrive;

  # Key material is staged on a ramfs (never a tmpfs) so it can never be
  # written out to swap.
  ramfs = name: "/luks-key-drive/${name}";

  mountPoint = name: "/luks-key-drive-media/${name}";

  keyPath = name: "${ramfs name}/key";

  headerPath = name: "${ramfs name}/header";

  # The unit the LUKS volume is opened by.  The volume names in use here
  # contain dashes, which systemd escapes as `\x2d`, so the name has to be run
  # through the same escaping systemd's generator uses or the ordering
  # silently doesn't apply.
  cryptsetupUnit = name: "systemd-cryptsetup@${utils.escapeSystemdPath name}.service";

  # Pull the key off the drive.  Without gpg this is just a copy; with gpg the
  # key is decrypted by an OpenPGP smartcard.  Either way the plaintext key
  # only ever lands on the ramfs.
  stageKey = name: opts:
    if opts.key.gpgPublicKey == null
    then ''
      cp ${mountPoint name}/${opts.key.file} ${keyPath name}
    ''
    else let
      gnupgHome = "${ramfs name}/.gnupg";
    in ''
      export GNUPGHOME=${gnupgHome}
      mkdir -m 0700 -p "$GNUPGHOME"

      ${pkgs.gnupg}/bin/gpg-agent \
        --daemon \
        --scdaemon-program ${pkgs.gnupg}/libexec/scdaemon \
        >/dev/null 2>/dev/null

      ${pkgs.gnupg}/bin/gpg --import ${opts.key.gpgPublicKey} >/dev/null 2>/dev/null

      # There is no passphrase on these volumes -- the key on the drive is the
      # only way in -- so there is nothing to fall back to and no point ever
      # giving up.  Keep asking until the key comes out.
      #
      # Any of the smartcards holding this key will do, so a card that has
      # locked itself after too many bad PINs is recovered by simply plugging
      # in another one.  scdaemon is killed and the card-specific key stubs are
      # dropped between attempts so the next card is picked up properly rather
      # than gpg insisting on the serial number it saw first.
      until [ -s ${keyPath name} ]; do
        echo -n "Waiting for an OpenPGP smartcard..."
        until ${pkgs.gnupg}/bin/gpg --card-status >/dev/null 2>/dev/null; do
          echo -n "."
          sleep 1
        done
        echo " found!"

        pin=$(systemd-ask-password --no-tty "Smartcard PIN for ${name}:")
        echo -n "$pin" | ${pkgs.gnupg}/bin/gpg \
          -q --batch --passphrase-fd 0 --pinentry-mode loopback \
          -d ${mountPoint name}/${opts.key.file} \
          >${keyPath name} 2>/dev/null || true
        unset pin

        if [ ! -s ${keyPath name} ]; then
          echo "Could not decrypt the key for ${name}."
          echo "Check the PIN, or plug in another card holding the same key."
          ${pkgs.gnupg}/bin/gpgconf --kill scdaemon >/dev/null 2>/dev/null || true
          rm -rf "$GNUPGHOME/private-keys-v1.d"
        fi
      done

      ${pkgs.gnupg}/bin/gpgconf --kill all >/dev/null 2>/dev/null || true
    '';
in {
  options.boot.initrd.luksWithKeyDrive = lib.mkOption {
    default = {};
    description = ''
      LUKS volumes whose key material lives on an external drive that has to be
      mounted from within the initrd.

      The drive is mounted read-only, the key (and the detached header, if any)
      are staged onto a ramfs, and the drive is unmounted again before the
      volume is opened, so nothing downstream depends on the drive still being
      present.

      These are regular `boot.initrd.luks.devices` entries -- this just fills in
      the key and header from the drive -- so anything else about the volume can
      still be set there.
    '';
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          device = lib.mkOption {
            type = lib.types.str;
            description = "The block device node of the encrypted volume.";
          };

          key = lib.mkOption {
            description = "How to get the key that opens this volume.";
            type = lib.types.submodule {
              options = {
                drive = lib.mkOption {
                  description = "The external drive holding the key.";
                  type = lib.types.submodule {
                    options = {
                      device = lib.mkOption {
                        type = lib.types.str;
                        description = "The block device node of the drive.";
                      };

                      fsType = lib.mkOption {
                        type = lib.types.str;
                        description = "The type of filesystem on the drive.";
                      };
                    };
                  };
                };

                file = lib.mkOption {
                  type = lib.types.str;
                  description = "Path to the key, relative to the root of the drive.";
                };

                header = lib.mkOption {
                  type = lib.types.nullOr lib.types.str;
                  default = null;
                  description = ''
                    Path to the detached LUKS header, relative to the root of
                    the drive.
                  '';
                };

                gpgPublicKey = lib.mkOption {
                  type = lib.types.nullOr lib.types.path;
                  default = null;
                  description = ''
                    If set, the key is gpg-encrypted and is decrypted by the
                    OpenPGP smartcard holding the matching private key, rather
                    than used as-is.
                  '';
                };
              };
            };
          };
        };
      }
    );
  };

  config = lib.mkIf (cfg != {}) {
    boot.initrd = {
      # usb_storage/uas for the drive itself, plus the filesystems it may
      # carry.  These are force-loaded rather than merely made available so the
      # drive is mountable without relying on autoloading in the initrd.
      kernelModules =
        ["usb_storage" "uas"]
        ++ lib.unique (lib.mapAttrsToList (_: opts: opts.key.drive.fsType) cfg);

      luks.devices =
        lib.mapAttrs (name: opts: {
          inherit (opts) device;
          keyFile = keyPath name;
          header = lib.mkIf (opts.key.header != null) (headerPath name);
        })
        cfg;

      systemd = {
        # gpg talks to the smartcard over usbfs via scdaemon's built-in CCID
        # driver, so there's no pcscd to bring along.  The public keys are
        # store paths the decrypt script reads, so they have to come along too.
        storePaths = lib.mkIf (lib.any (opts: opts.key.gpgPublicKey != null) (lib.attrValues cfg)) (
          [
            "${pkgs.gnupg}/bin/gpg"
            "${pkgs.gnupg}/bin/gpg-agent"
            "${pkgs.gnupg}/bin/gpgconf"
            "${pkgs.gnupg}/libexec/scdaemon"
          ]
          ++ (
            lib.mapAttrsToList (_: opts: opts.key.gpgPublicKey)
            (lib.filterAttrs (_: opts: opts.key.gpgPublicKey != null) cfg)
          )
        );

        services =
          lib.mapAttrs' (
            name: opts:
              lib.nameValuePair "luks-key-drive-${name}" {
                description = "Stage LUKS key material for ${name} from an external drive";

                # Both are needed: `before` alone does not pull the unit in.
                wantedBy = [(cryptsetupUnit name)];
                before = [
                  (cryptsetupUnit name)
                  "initrd-switch-root.target"
                  "shutdown.target"
                ];
                after = [
                  "systemd-modules-load.service"
                  "systemd-udevd.service"
                ];
                conflicts = [
                  "initrd-switch-root.target"
                  "shutdown.target"
                ];
                unitConfig.DefaultDependencies = "no";

                serviceConfig = {
                  Type = "oneshot";
                  RemainAfterExit = true;

                  # The drive and smartcard waits are deliberately unbounded,
                  # so systemd must not put its own deadline on them either.
                  TimeoutStartSec = "infinity";
                };

                script = ''
                  mkdir -p ${ramfs name} ${mountPoint name}
                  mount -t ramfs none ${ramfs name}
                  umask 277

                  echo -n "Waiting for the key drive for ${name} to appear..."
                  until [ -e ${opts.key.drive.device} ]; do
                    echo -n "."
                    sleep 1
                  done
                  echo " found!"

                  mount -t ${opts.key.drive.fsType} -o ro ${opts.key.drive.device} ${mountPoint name}

                  ${stageKey name opts}
                  ${lib.optionalString (opts.key.header != null) ''
                    cp ${mountPoint name}/${opts.key.header} ${headerPath name}
                  ''}

                  # The volume is opened from the ramfs copies, so the drive is
                  # free to go before cryptsetup runs.
                  umount ${mountPoint name}
                '';

                postStop = ''
                  umount ${mountPoint name} 2>/dev/null || true
                  umount ${ramfs name} 2>/dev/null || true
                '';
              }
          )
          cfg;
      };
    };
  };
}
