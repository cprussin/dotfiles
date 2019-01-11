{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      setup-monitors = super.callPackage ./package.nix {};
    })
  ];

  systemd.user.services.watch-monitor-change = {
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

  systemd.user.services.setup-monitors = {
    Unit = {
      Description = "Re-configure monitors";
      After = [ "graphical-session-pre.target" ];
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
}
