{
  lib,
  config,
  ...
}: let
  cfg = config.autologin-graphical-session;
in {
  options.autologin-graphical-session = {
    enable = lib.mkEnableOption "Autologin graphical session";

    user = lib.mkOption {
      type = lib.types.str;
    };

    sessionScript = lib.mkOption {
      type = lib.types.path;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd = {
      defaultUnit = "graphical.target";

      services.autologin-graphical-session = {
        description = "Autologin graphical session";
        wantedBy = ["graphical.target"];
        aliases = ["display-manager.service"];
        after = [
          "systemd-user-sessions.service"
          "getty@tty1.service"
        ];
        before = ["plymouth-quit.service"];
        conflicts = ["getty@tty1.service"];

        environment.XDG_RUNTIME_DIR = "/run/user/${toString config.users.users.${cfg.user}.uid}";

        script = ''
          . /etc/set-environment
          . /etc/profiles/per-user/${cfg.user}/etc/profile.d/hm-session-vars.sh
          exec ${cfg.sessionScript}
        '';

        serviceConfig = {
          WorkingDirectory = config.users.users.${cfg.user}.home;

          PAMName = "login";
          User = cfg.user;

          UtmpIdentifier = "tty1";
          TTYPath = "/dev/tty1";
          TTYReset = "yes";
          TTYVTDisallocate = "yes";

          SyslogIdentifier = "autologin-graphical-session";
          StandardInput = "tty";
          StandardError = "journal";
          StandardOutput = "journal";

          Restart = "always";
          RestartSec = "2";
          Nice = "-5";
        };
      };
    };
  };
}
