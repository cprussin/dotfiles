# Claude Code's background daemon, so claude.ai can start sessions on this
# machine without anyone running `claude remote-control` by hand.
#
# Claude Desktop can't do it here: its Remote Control hosting is behind a
# server-side gate, and on Linux it says "The Claude app can't connect a folder
# on this device yet" (tested on lyra with dotfiles#304 deployed).
#
# The CLI has its own: `claude daemon` supervises Remote Control servers listed
# in ~/.claude/daemon.json.  `claude daemon install` would write this same unit
# itself, but pointing at whichever store path ran it -- gone after the next
# garbage collection, and never upgraded.  So it is declared here instead,
# under the name and with the ExecStart shape the CLI looks for: it treats the
# daemon as installed when ~/.config/systemd/user/com.anthropic.claude-daemon
# .service exists, and only then lets `claude daemon remote-control add` run.
#
# Which folders it serves is the CLI's state, not this file's: it lives in
# ~/.claude/daemon.json, which is persisted with the rest of ~/.claude.
#
# ~/.claude.json is not: it sits beside ~/.claude on the tmpfs root, so the
# account and folder-trust state in it went with every reboot.  With
# CLAUDE_CONFIG_DIR set the CLI keeps that file inside the directory instead,
# where it persists -- set for the daemon and for every login alike, so both
# read and write the same one (after a re-login: sessionVariables only reach
# new sessions).
#
# Once, after the switch and a re-login:
#
#   cp ~/.claude.json ~/.claude/.claude.json   # keep this boot's state, if any
#   cd ~ && claude                               # accept the trust dialog
#   claude daemon remote-control add --dir ~
#
# A folder must be trusted before it can be served, and with nothing added
# the daemon serves nothing and the machine shows offline to claude.ai.
{
  config,
  pkgs,
  ...
}: let
  home = "/home/${config.primary-user.name}";
  configDir = "${home}/.claude";
in {
  # For `claude daemon remote-control add` and friends.  systemPackages rather
  # than home.packages, as config/machines/crux/claude-agent.nix explains.
  environment = {
    systemPackages = [pkgs.claude-code];
    sessionVariables.CLAUDE_CONFIG_DIR = configDir;
  };

  primary-user.home-manager.systemd.user.services."com.anthropic.claude-daemon" = {
    Unit = {
      Description = "Claude Daemon";
      StartLimitIntervalSec = 60;
      StartLimitBurst = 10;
    };

    Service = {
      Type = "simple";
      Environment = ["CLAUDE_CONFIG_DIR=${configDir}"];
      ExecStart = "${pkgs.claude-code}/bin/claude daemon --json-path ${configDir}/daemon.json --log-file ${configDir}/daemon.log --origin service";
      Restart = "always";
      RestartSec = 1;
    };

    Install.WantedBy = ["default.target"];
  };
}
