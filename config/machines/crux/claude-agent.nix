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
# `claude --remote-control` starts an *interactive* session -- the flag's own
# help says so -- so there is nothing to run under systemd.  A unit with no
# terminal would either fail or sit there being useless, and a unit that
# pretended otherwise would be a lie in a file people trust.
#
# Something has to make that session outlive an ssh disconnect, but it is not
# this module: whoever connects to this machine already lands in tmux, and a
# wrapper that started its own would nest one inside it -- two prefix keys deep
# for no gain.  So `claude-agent` is a `cd` and an `exec`, and the session it
# starts belongs to whatever the caller is already running under.
#
# THE ONE THING THAT COSTS.  It used to be `tmux new-session -A`, which
# reattached rather than starting a second agent, and a second
# `--remote-control` session is a second agent on the same build tree -- the
# collision the next section is about.  That is documented now rather than
# enforced: run `claude-agent` twice and you get two.  Enforcing it without a
# multiplexer would take a lock file, which is machinery this module has not
# been asked for.
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

  # `cd` and `exec`, and that is deliberately all of it.  What this buys over
  # typing the command is the working directory and the session name, both of
  # which are easy to get wrong and neither of which is worth a wrapper that
  # does anything else.
  #
  # `exec` so the shell is replaced rather than left waiting: a signal reaches
  # the agent, and the exit status is the agent's.
  claude-agent = pkgs.writeShellScriptBin "claude-agent" ''
    set -eu
    cd "${workDir}"
    exec ${pkgs.claude-code}/bin/claude --remote-control crux
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
