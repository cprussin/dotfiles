# A GitHub Actions runner for cprussin/domicile, so CI can build the engine.
#
# Domicile carries a Chromium fork.  A clean build of it is 4h16m and 97G --
# see chromium-build.nix, which owns /build and measured both -- and an
# incremental one against that tree is 65s.  No GitHub-hosted runner can hold
# the tree: the Actions cache is 10G per repository and the runners' disks are
# smaller than the checkout, so every run there would be the 4h16m and not the
# 65s.  This machine already has the warm tree, which is the only reason the
# fast number exists at all.
#
# What that buys is not really CI.  It is that a change to the engine can be
# compiled and its pixel assertion run without a person carrying a git bundle
# between two machines, which is how it has been done until now.
#
#
# THIS RUNS AS THE PRIMARY USER, AND THAT IS A REAL TRADE.
#
# /build is the primary user's and the warm tree in it is the whole point.  A
# dedicated service user would need either its own 97G tree -- the dataset's
# quota is 200G, so two do not fit -- or group-write on that one, which does
# not survive the default umask: files one user creates come out
# group-readable and not group-writable, and the next incremental build by the
# other user fails on them.  So the runner is the same user that builds
# interactively.
#
# The consequence is that anything this runner executes runs as that user.
# **The compensating control is not in this file**: fork pull requests must
# not be allowed to run on it.  That is a GitHub repository setting -- Actions
# -> "Require approval for all external contributors" -- plus keeping the
# engine workflow on `push` and `workflow_dispatch` rather than
# `pull_request`.  Nothing here can enforce either, which is exactly why it is
# written down here.
#
#
# ONE RUNNER, DELIBERATELY.
#
# A second instance on this machine would share /build/chromium with the
# first, and two concurrent `autoninja` runs against one out/ directory
# corrupt each other.  One runner is one job at a time, which is the
# serialisation, for free.  The workflow should also carry a `concurrency`
# group so queued runs collapse rather than pile up.
{
  config,
  pkgs,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};

  # Kept next to the build tree rather than under /var/lib: a job checks out
  # the repository and then runs cargo and bun in it, and that scratch belongs
  # on the NVMe beside the rest of the build rather than on the root pool.
  #
  # It counts against the same 200G quota the Chromium tree does.  The
  # checkout is small -- the repository is a few megabytes -- but its target/
  # and node_modules are not, so this is worth remembering if a build starts
  # failing on space rather than on code.
  workDir = "/build/github-runner";
in {
  deployment.keys.domicile-github-runner-token = {
    # A personal access token rather than a runner registration token: the
    # registration kind expires in an hour, so a rebuild any later than that
    # would fail to re-register and the runner would simply stop existing.  A
    # PAT lets the service mint its own registration token whenever it starts.
    #
    # Fine-grained, scoped to cprussin/domicile alone, with Administration:
    # read and write -- which is what registering a runner needs and is the
    # whole of what it needs.
    keyCommand = passwords.getPassword "Connor/Infrastructure/github/domicile-runner-token";
    destDir = "/secrets";
    user = config.primary-user.name;
  };

  services.github-runners.domicile = {
    enable = true;
    url = "https://github.com/cprussin/domicile";
    tokenFile = config.deployment.keys.domicile-github-runner-token.path;

    # The name the workflow selects on, via `runs-on`.  Named for the machine
    # rather than for the job, because what makes it special is this machine's
    # warm tree and its GPU, and a second workflow wanting either should say
    # the same word.
    name = "crux";
    extraLabels = ["crux" "chromium" "gpu"];

    # Not ephemeral.  An ephemeral runner de-registers after every job, which
    # is the right posture for a runner that starts from nothing -- and this
    # one exists precisely because it does not.
    ephemeral = false;
    replace = true;

    user = config.primary-user.name;
    inherit workDir;

    # `nix` is what the workflow enters Chromium's own toolchain shell with
    # (`nix develop "path:./tools/nix"`); the rest are what depot_tools and
    # the repository's own checks reach for.  A service's PATH is not a login
    # shell's, so anything the workflow runs by bare name has to be here.
    extraPackages = with pkgs; [
      bash
      cacert
      coreutils
      curl
      git
      gnutar
      gzip
      lsb-release
      nix
      python3
      which
    ];

    extraEnvironment = {
      # **Load-bearing, and the reason is in chromium-build.nix**: /tmp is
      # tmpfs, so it is RAM, and a Chromium link there takes the machine out.
      # That file sets this for login sessions through
      # environment.sessionVariables, which a systemd service does not read --
      # so a runner without this line would link in RAM and the first symptom
      # would be the house's DNS server falling over.
      TMPDIR = "/build/tmp";
    };

    serviceOverrides = {
      # The engine's pixel assertion drives a real GPU client against a real
      # render node -- that is what it is for -- so the service needs the
      # groups /dev/dri is owned by.  Without these the guard fails with no
      # EGL renderer, which reads as a broken change rather than a broken
      # runner.
      SupplementaryGroups = ["render" "video"];

      # The upstream unit is hardened, and two of those defaults would take
      # the GPU and the build tree away.  Turned off with the reason rather
      # than wholesale: everything else the module sets still applies.
      PrivateDevices = false;
      ReadWritePaths = ["/build"];
    };
  };
}
