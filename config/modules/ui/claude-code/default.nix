# Keeps `claude remote-control` running in ~/Scratch, so claude.ai can start
# sessions on this machine without anyone running it by hand.
#
# Not Claude Desktop: its Remote Control hosting is server-gated off on Linux
# ("The Claude app can't connect a folder on this device yet", dotfiles#304).
#
# Not `claude daemon` (dotfiles#307): `claude daemon remote-control add` is
# behind the server-side flag `tengu_radiant_heron` (claude-code 2.1.322), and
# says "not available in this environment" when it is off.  Prod builds stub
# out every flag override, so that is not fixable here.
#
# Plain `remote-control` is not gated and runs fine without a terminal, once
# ~/Scratch is trusted and "Enable Remote Control?" has been answered (both need
# a terminal, once: `cd ~/Scratch; claude remote-control`, answer y).
#
# CLAUDE_CONFIG_DIR keeps ~/.claude.json inside the persisted ~/.claude rather
# than on the tmpfs root, for this unit and every login alike, so both share
# the account and folder trust.
{
  config,
  pkgs,
  ...
}: let
  user = config.primary-user.name;
  home = "/home/${user}";
  configDir = "${home}/.claude";
  dir = "${home}/Scratch";
in {
  # systemPackages rather than home.packages, as
  # config/machines/crux/claude-agent.nix explains.
  environment = {
    systemPackages = [pkgs.claude-code];
    sessionVariables.CLAUDE_CONFIG_DIR = configDir;
  };

  primary-user.home-manager.systemd.user.services.claude-remote-control = {
    Unit = {
      Description = "Claude Code Remote Control in ~/Scratch";
      ConditionPathIsDirectory = dir;
    };

    Service = {
      Type = "simple";
      WorkingDirectory = dir;
      Environment = [
        "CLAUDE_CONFIG_DIR=${configDir}"
        # Sessions it spawns get the same tools a login shell has.
        "PATH=/run/wrappers/bin:/etc/profiles/per-user/${user}/bin:/run/current-system/sw/bin"
      ];
      ExecStart = "${pkgs.claude-code}/bin/claude remote-control --no-create-session-in-dir";
      # Its stdout is a terminal UI that redraws several times a second, which
      # filled the journal. Its startup errors, such as being signed out, go to
      # stderr and still reach the journal, as does its exit status.
      StandardOutput = "null";
      # Signed out, offline at boot, or a crash: keep retrying, gently.
      Restart = "always";
      RestartSec = 15;
    };

    Install.WantedBy = ["default.target"];
  };
}
