{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (
      _: super: {
        setup-monitors = super.callPackage ./package.nix {};
      }
    )
  ];

  services.udev.extraRules = ''
    KERNEL=="card0", SUBSYSTEM=="drm", ACTION=="change", RUN+="${pkgs.coreutils}/bin/touch /run/display-change"
  '';

  system.activationScripts.displayChange = ''
    umask 022
    touch /run/display-change
  '';

  home-manager.users.${config.primaryUserName}.systemd.user.services = {
    watch-monitor-change = {
      Unit = {
        Description = "Watch for monitor hotplugging and re-configure monitors";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = "${pkgs.setup-monitors}/bin/watch-monitor-change";
      };
    };

    setup-monitors = {
      Unit = {
        Description = "Re-configure monitors";
        After = [ "graphical-session-pre.target" ];
        Before = [ "random-background.service" ];
        Wants = [ "random-background.service" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };

      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.setup-monitors}/bin/setup-monitors";
      };
    };
  };
}
