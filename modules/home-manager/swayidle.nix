{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.services.swayidle;

  stringifyTimeout = timeout: let
    resumeString =
      lib.optionalString (timeout.resume != null) " resume '${timeout.resume}'";
  in "timeout ${toString timeout.timeout} '${timeout.cmd}'${resumeString}";
in {
  options.services.swayidle = {
    enable = lib.mkEnableOption "Swayidle";

    timeouts = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            timeout = lib.mkOption {
              type = lib.types.int;
              description = ''
                Run the command after this number of seconds with no activity.
              '';
            };
            cmd = lib.mkOption {
              type = lib.types.oneOf [lib.types.str lib.types.path];
              description = "The command to run.";
            };
            resume = lib.mkOption {
              type = lib.types.nullOr (lib.types.oneOf [lib.types.str lib.types.path]);
              description = ''
                This command will be run when there is activity again.
              '';
              default = null;
            };
          };
        }
      );
      default = [];
    };

    beforeSleep = lib.mkOption {
      type = lib.types.nullOr (lib.types.oneOf [lib.types.str lib.types.path]);
      description = ''
        Executes this command before systemd puts the computer to sleep.

        Note: this only delays sleeping up to the limit set in logind.conf(5) by
        the option InhibitDelayMaxSec. A command that has not finished by then
        will continue running after resuming from sleep.
      '';
      default = null;
    };

    afterResume = lib.mkOption {
      type = lib.types.nullOr (lib.types.oneOf [lib.types.str lib.types.path]);
      description = ''
        Executes this command after logind signals that the computer resumed
        from sleep.
      '';
      default = null;
    };

    lock = lib.mkOption {
      type = lib.types.nullOr (lib.types.oneOf [lib.types.str lib.types.path]);
      description = ''
        Executes this command when logind signals that the session should be
        locked.
      '';
      default = null;
    };

    unlock = lib.mkOption {
      type = lib.types.nullOr (lib.types.oneOf [lib.types.str lib.types.path]);
      description = ''
        Executes this command when logind signals that the session should be
        unlocked.
      '';
      default = null;
    };

    idlehint = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      description = ''
        Set IdleHint to indcate an idle logind/elogind session after this number
        of seconds. Adding an idlehint event will also cause swayidle to call
        SetIdleHint(false) when run, on resume, unlock, etc.
      '';
      default = null;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.swayidle = {
      Unit = {
        Description = "Swayidle screen locker";
        PartOf = ["graphical-session.target"];
        After = ["graphical-session.target"];
      };
      Install.WantedBy = ["graphical-session.target"];
      Service.ExecStart = builtins.concatStringsSep " " (
        [
          "${pkgs.swayidle}/bin/swayidle -w"
          (lib.optionalString (cfg.beforeSleep != null) "before-sleep ${cfg.beforeSleep}")
          (lib.optionalString (cfg.afterResume != null) "after-resume ${cfg.afterResume}")
          (lib.optionalString (cfg.lock != null) "lock ${cfg.lock}")
          (lib.optionalString (cfg.unlock != null) "unlock ${cfg.unlock}")
          (lib.optionalString (cfg.idlehint != null) "idlehint ${cfg.unlock}")
        ]
        ++ (map stringifyTimeout cfg.timeouts)
      );
    };
  };
}
