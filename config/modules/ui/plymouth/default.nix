{
  config,
  lib,
  pkgs,
  ...
}: let
  plymouth = "${config.boot.plymouth.package}/bin/plymouth";

  # How long the display manager's desktop gets to put up its first frame
  # before plymouth goes.  Generous on purpose: once the desktop has drawn,
  # plymouth's last frame is off the screen and quitting it is invisible, but
  # quitting first drops back to the bare console until the desktop draws.
  handOffWithin = 30;
in {
  boot = {
    plymouth = {
      enable = true;
      theme = "breeze";
    };

    # Nothing on the console in front of the splash: no kernel messages below
    # errors, no initrd or udev chatter, and systemd's status only on failure.
    consoleLogLevel = 3;
    initrd.verbose = false;
    kernelParams = ["quiet" "udev.log_level=3" "systemd.show_status=auto"];
  };

  # Hand the screen straight from the splash to the desktop, as GDM does.  By
  # default plymouth quits before the display manager starts, leaving a blank
  # console until the desktop draws.  Instead the display manager starts with
  # plymouth still up: `deactivate` lets go of the card but leaves its last
  # frame on screen until the desktop's first modeset replaces it, and
  # `--retain-splash` keeps that frame when plymouth then exits.
  systemd.services = lib.mkIf config.services.displayManager.enable {
    display-manager = {
      conflicts = ["plymouth-quit.service"];
      # A display manager that never comes up shows the console, to debug it
      # from.
      onFailure = ["plymouth-quit.service"];
    };

    # As nixpkgs' GDM module does: a switch starts `multi-user.target`, which
    # would start `plymouth-quit` and so stop the display manager.
    plymouth-quit.wantedBy = lib.mkForce [];

    plymouth-hand-off = {
      description = "Quit plymouth once the desktop has the screen";
      wantedBy = ["display-manager.service"];
      after = ["display-manager.service"];
      serviceConfig.Type = "oneshot";
      script = ''
        ${pkgs.coreutils}/bin/sleep ${toString handOffWithin}
        # Nothing to quit when the display manager restarts later in the boot.
        ${plymouth} quit --retain-splash || true
      '';
    };
  };

  # Fails when plymouth is already gone, as when the display manager restarts.
  services.displayManager.generic.preStart = lib.mkIf config.services.displayManager.enable ''
    ${plymouth} deactivate || true
  '';
}
