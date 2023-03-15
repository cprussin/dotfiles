{
  lib,
  pkgs,
  ...
}: let
  sources = import ../sources.nix;

  solarized-dark = pkgs.callPackage ../lib/color-themes/solarized/dark.nix {};

  gpg-conf = pkgs.writeText "gpg.conf" "no-symkey-cache";

  update-key-expiry = pkgs.writeShellScriptBin "update-key-expiry" ''
    echo "Step 1: Update gpg-agent.conf to use correct pinentry (maybe we can skip this though?)"
    echo "Step 2: Update expiry on subkeys (add selection)"
    echo "Step 3: Export public key to a place on liveusb"
    echo "Step 4: Update master zfs dataset"
    echo "Step 5: Scrub master zfs dataset"
    echo "Step 6: Sync to backup zfs datasets"
    echo "Step 7: Scrub backup zfs datasets"
  '';
in {
  nixpkgs = {
    config.allowBroken = true;
    overlays = [
      (
        import ../pkgs/dircolors-solarized/overlay.nix {
          src = sources.dircolors-solarized;
        }
      )
    ];
  };

  isoImage = {
    isoBaseName = lib.mkForce "gpg-offline";
    appendToMenuLabel = " GPG Offline System";
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = ["copytoram"];
    cleanTmpDir = true;
    kernel.sysctl."kernel.unprivileged_bpf_disabled" = 1;
    initrd.network.enable = false;
  };

  services = {
    pcscd.enable = true;
    gpm.enable = true;
    openssh.enable = lib.mkForce false;
    udev.packages = [pkgs.yubikey-personalization];
    getty = {
      greetingLine = lib.mkForce "> GPG Offline System <";
      helpLine = lib.mkForce "Useful commands:\n  - `update-key-expiry`";
    };
  };

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      pinentryFlavor = "curses";
      enableSSHSupport = true;
    };
  };

  networking = {
    dhcpcd = {
      enable = false;
      allowInterfaces = [];
    };
    interfaces = {};
    firewall.enable = true;
    useDHCP = false;
    useNetworkd = false;
    wireless.enable = false;
    networkmanager.enable = lib.mkForce false;
  };

  console = {
    keyMap = pkgs.runCommand "console-keymap" {} ''
      ${pkgs.ckbcomp}/bin/ckbcomp \
        -layout us \
        -option caps:escape \
        -variant dvp > "$out"
    '';
    colors = map (builtins.replaceStrings ["#"] [""]) [
      solarized-dark.background
      solarized-dark.red
      solarized-dark.green
      solarized-dark.yellow
      solarized-dark.blue
      solarized-dark.purple
      solarized-dark.cyan
      solarized-dark.foreground
      solarized-dark.grey
      solarized-dark.lightRed
      solarized-dark.lightGreen
      solarized-dark.lightYellow
      solarized-dark.lightBlue
      solarized-dark.lightPurple
      solarized-dark.lightCyan
      solarized-dark.white
    ];
  };

  environment = {
    systemPackages = [
      (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.hopenpgp-tools)
      pkgs.cfssl
      pkgs.cryptsetup
      pkgs.diceware
      pkgs.ent
      pkgs.gptfdisk
      pkgs.paperkey
      pkgs.pcsctools
      pkgs.pgpdump
      pkgs.pwgen
      pkgs.yubico-piv-tool
      pkgs.yubikey-manager
      pkgs.yubikey-personalization
      update-key-expiry
    ];

    etc."inputrc".text = "set editing-mode vi";

    interactiveShellInit = ''
      unset HISTFILE

      export GNUPGHOME="$HOME/.gnupg"
      [ ! -d "$GNUPGHOME" ] && install -m=0700 --directory "$GNUPGHOME"
      [ ! -f "$GNUPGHOME/gpg.conf" ] && cp ${gpg-conf} "$GNUPGHOME/gpg.conf"

      echo "\$GNUPGHOME is \"$GNUPGHOME\""
    '';
  };
}
