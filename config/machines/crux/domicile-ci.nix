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
  lib,
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
  };

  # /build is chromium-build.nix's, and its rules are redone after the mount by
  # setup-build-mount.service -- a rule laid down at sysinit lands on the tmpfs
  # root and is then hidden by build.mount.  That service runs
  # `systemd-tmpfiles --create --prefix=/build`, so a rule added here is picked
  # up by it and needs nothing of its own.
  #
  # Without this the unit fails twice on a first deploy and stays failed: its
  # root ExecStartPre runs `find -H "$WORK_DIRECTORY" -mindepth 1 -delete`
  # under `set -euo pipefail`, and BindPaths names the directory unprefixed,
  # which systemd treats as fatal when the source is missing.
  systemd.tmpfiles.rules = [
    "d ${workDir} 0755 ${config.primary-user.name} users -"
  ];

  systemd.services.github-runner-domicile = {
    # Colmena writes the token asynchronously and generates a -key.service as
    # the gate on it; every other key consumer in this repository pairs both,
    # and this one has to as well.  Without it a start before the key lands
    # fails on `install ... ${tokenFile}` and, since a non-ephemeral runner is
    # Restart=no by default, never comes back.
    requires = ["domicile-github-runner-token-key.service"];

    # /build is a nofail mount, so nothing waits for it on its own, and
    # setup-build-mount.service is what checks that what is mounted there is
    # actually the dataset.  chromium-build.nix wrote that check because
    # TMPDIR pointing at a tmpfs root is what takes this machine out -- and
    # this unit sets exactly that TMPDIR.
    after = ["domicile-github-runner-token-key.service" "setup-build-mount.service"];
    unitConfig.RequiresMountsFor = "/build";
  };

  services.github-runners.domicile = {
    enable = true;
    url = "https://github.com/cprussin/domicile";
    tokenFile = config.deployment.keys.domicile-github-runner-token.path;

    # `runs-on` matches *labels*, not this -- the name is what identifies the
    # runner in the repository's settings.  It is spelled the same as the label
    # below on purpose, so that what a workflow asks for and what shows up in
    # that list read alike.
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
    # Only what the module does not already put on PATH -- it supplies bash,
    # coreutils, git, tar, gzip and nix itself, and naming them again just puts
    # them there twice.  `cacert` would be worse than redundant: it is a
    # package with no binaries, so it adds a PATH entry and no trust, and TLS
    # trust here comes from /etc/ssl.
    extraPackages = with pkgs; [
      curl
      lsb-release # depot_tools probes it
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

    # The upstream unit is hardened for a runner that compiles ordinary code.
    # This one drives Chromium's own build sandbox and a GPU, and six of those
    # defaults forbid exactly that.  Each is turned off with its reason;
    # everything else the module sets still applies.
    #
    # mkForce throughout because serviceOverrides goes through the module
    # system: a plain value conflicts with the module's own rather than
    # replacing it.
    serviceOverrides = {
      # THE BUILD SANDBOX.  Chromium is built inside upstream's own nix shell,
      # which is a buildFHSEnv -- and in nixpkgs that is buildFHSEnvBubblewrap,
      # whose shellHook execs bwrap.  bwrap unshares a mount namespace and a
      # user namespace and then mounts inside them, so RestrictNamespaces,
      # ~@mount and PrivateUsers each kill it in the shellHook, before anything
      # it was asked to run.  That is the one thing this runner exists to do.
      RestrictNamespaces = lib.mkForce false;
      PrivateUsers = lib.mkForce false;
      SystemCallFilter = lib.mkForce [];

      # THE GPU.  The pixel assertion drives a real client against a real
      # render node.  PrivateDevices=false is necessary and not sufficient:
      # ProtectClock implies DeviceAllow=char-rtc, and any explicit DeviceAllow
      # turns the policy from "everything" to "only what is listed", so the
      # module's own empty reset plus that implication is a closed policy.
      #
      # Cleared rather than enumerated.  crux runs the proprietary NVIDIA
      # driver, whose EGL path opens /dev/nvidia0, /dev/nvidiactl and
      # /dev/nvidia-uvm besides /dev/dri -- those are root:root 0666, so the
      # groups below do nothing for them -- and a list that misses one fails as
      # "no EGL renderer", which reads as a broken change rather than a missing
      # device.  Narrow it once something has confirmed which nodes are opened.
      PrivateDevices = false;
      ProtectClock = lib.mkForce false;
      DeviceAllow = lib.mkForce [];

      # /dev/dri's own nodes are group-owned, so these are still needed.
      SupplementaryGroups = ["render" "video"];

      # ProtectSystem=strict with no ReadWritePaths of the module's own, so
      # /build/chromium and /build/tmp would be read-only.  HOME is the
      # workDir, which the module already bind-mounts read-write.
      ReadWritePaths = ["/build"];

      # A non-ephemeral runner is Restart=no upstream, which is right for a
      # runner whose failure means its token is gone.  Here the likelier cause
      # is that /build was not ready yet, and a unit that stays dead until
      # somebody notices is worse than one that tries again.
      Restart = lib.mkForce "on-failure";
      RestartSec = 30;
    };
  };
}
