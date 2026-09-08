# Claude Code on crux, so an agent can work on the engine where the tree is.
#
# Domicile carries a Chromium fork.  Everything about working on it is shaped
# by one fact, which chromium-build.nix measured: a clean build is 4h16m and
# 97G, and an incremental one against the warm tree here is 65s.  An agent
# working anywhere else writes browser-process C++ from memory and finds out
# four hours later whether it compiles; an agent here reads the real headers at
# the pinned revision and finds out in a minute.
#
# That is the whole reason this exists.  It is the same argument domicile-ci.nix
# makes for the CI runner, one step further: CI proves a change compiles, and
# this is for *writing* the change in the first place.
#
#
# WHY THERE IS NO SERVICE HERE, AND NO TERMINAL MULTIPLEXER EITHER.
#
# `claude rc` starts an *interactive* Remote Control session, so there is
# nothing to run under systemd.  A unit with no terminal would either fail or
# sit there being useless, and a unit that pretended otherwise would be a lie
# in a file people trust.
#
# `rc` rather than the `--remote-control crux` this used to pass: it is the
# subcommand for the same thing, and it takes the session name from the
# hostname (see `--remote-control-session-name-prefix`, whose default is the
# hostname), which on this machine is the name that was being passed by hand.
# Worth a line because `rc` does not appear in `claude --help` -- it resolves,
# and an unknown subcommand there says so, but nobody will find it by reading
# the help.
#
# Something has to make that session outlive an ssh disconnect, but it is not
# this module: whoever connects to this machine already lands in tmux, and a
# wrapper that started its own would nest one inside it -- two prefix keys deep
# for no gain.  So `claude-agent` is a `cd` and an `exec`, and the session it
# starts belongs to whatever the caller is already running under.
#
# WHAT TOOK OVER FROM `new-session -A`.  Reattaching had a second job besides
# convenience: it made a second agent impossible, and a second
# Remote Control session is a second agent on the same build tree -- the
# collision the next section is about.  Without a multiplexer that has to be a
# lock, so it is one.
#
# `flock -n` takes an exclusive lock and fails immediately rather than waiting,
# so the second `claude-agent` refuses and says where the first one is instead
# of quietly becoming a second writer.  The lock lives on an open file
# descriptor, and descriptors survive `exec` -- so the agent process holds it
# for its whole life and the kernel drops it when that process dies.  There is
# no stale lock to clean up after a crash, which is the failure mode a pid file
# would have had.
#
# IT GUARDS AGENT AGAINST AGENT, AND NOTHING ELSE.  The CI runner does not take
# this lock and cannot be made to from here, so it is `DOMICILE_AGENT_OUT` that
# keeps the two apart -- see below.  Saying which half a guard covers is worth
# more than the guard.
#
#
# IT SHARES /build WITH THE CI RUNNER, AND THAT IS THE SHARP EDGE.
#
# domicile-ci.nix says it plainly for its own case: two concurrent `autoninja`
# runs against one out/ directory corrupt each other.  The runner uses
# out/Domicile, because that is what packages/domicile-engine/scripts/build.sh
# defaults to.  So an agent that builds must not use out/Domicile, and that is
# what DOMICILE_AGENT_OUT is for -- build.sh takes OUT from the environment.
#
# The cost is disk.  A second out/ is not free against the 200G quota the
# Chromium tree already takes 97G of, and it is the reason this is a documented
# variable rather than a second tree.
#
# Worse than a corrupt build, and worth stating because it happened: a failed
# `gn gen` writes args.gn before the assert that failed it, and ninja re-runs
# gn on every build.  One bad `gn gen` in a shared out/ leaves *every* branch's
# CI failing with `rebuild manifest failed`, which reads as a code error and is
# not one.
#
#
# IT RUNS AS THE PRIMARY USER, WHICH IS MORE THAN THE RUNNER ASKS FOR.
#
# domicile-ci.nix already accepts that trade for CI, and its compensating
# control is that fork pull requests cannot run on it -- a workflow file is
# reviewed before it executes.  An interactive agent has no such gate: whoever
# is driving it can run anything this user can, on a machine that also serves
# dns, matrix, home-assistant and the backups.
#
# There is no technical control here for that, and pretending otherwise would
# be worse than saying so.  The control is that a person starts it deliberately
# and closes it when the work is done.  It is not enabled at boot and nothing
# starts it automatically, which is the only part this file can enforce.
{
  config,
  pkgs,
  ...
}: let
  # Where an agent checks Domicile out.  Beside the Chromium tree rather than
  # in the primary user's home, for the reason domicile-ci.nix puts its own
  # workspace here: the NVMe dataset is where the build artifacts belong, and
  # cargo and bun scratch is build artifacts.
  #
  # Not shared with /build/github-runner, which the runner's ExecStartPre
  # deletes the contents of on every start -- an agent's uncommitted work would
  # go with it.
  workDir = "/build/claude-agent";

  # The lock, in the per-user runtime directory: a tmpfs the system clears
  # between logins, so nothing accumulates.  `/tmp` only as a fallback, since
  # XDG_RUNTIME_DIR is not guaranteed to be set over ssh, and named by uid
  # there because /tmp is shared and a fixed name in it is another user's to
  # take.
  #
  # `exec` so the shell is replaced rather than left waiting on a child: a
  # signal reaches the agent, and the exit status is the agent's.
  claude-agent = pkgs.writeShellScriptBin "claude-agent" ''
    set -eu

    lock="''${XDG_RUNTIME_DIR:-/tmp}/claude-agent.$(${pkgs.coreutils}/bin/id -u).lock"
    # Held on the descriptor, not by the file existing, so this survives the
    # `exec` below and is released by the kernel when the agent exits --
    # crash included.
    exec 9>"$lock"
    ${pkgs.util-linux}/bin/flock -n 9 || {
      echo "claude-agent: one is already running on this machine." >&2
      echo "  A second would be a second agent on ${workDir} and on the" >&2
      echo "  Chromium tree beside it, which is the collision this refuses." >&2
      echo "  Attach to the session you already have, or stop it first." >&2
      exit 1
    }

    cd "${workDir}"
    exec ${pkgs.claude-code}/bin/claude rc --no-create-session-in-dir
  '';
in {
  environment = {
    # `environment.systemPackages`, deliberately, and not the primary user's
    # `home.packages`: config/modules/ui/session sets that with `lib.mkForce`,
    # so a second module adding to it either loses or clobbers depending on
    # which the module system sees last.
    systemPackages = [pkgs.claude-code claude-agent];

    # What build.sh should be told, so an agent's build never lands in the
    # runner's out/Domicile.  Exported for every login rather than left in a
    # README, because the failure it prevents is silent for the *other* user of
    # the tree -- CI goes red on branches nobody touched.
    variables.DOMICILE_AGENT_OUT = "out/Agent";
  };

  systemd.tmpfiles.rules = [
    "d ${workDir} 0755 ${config.primary-user.name} users -"
  ];
}
