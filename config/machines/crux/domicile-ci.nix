# GitHub Actions runners for cprussin/domicile, so CI can build the engine.
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
# THESE RUN AS THE PRIMARY USER, AND THAT IS A REAL TRADE.
#
# /build is the primary user's and the warm tree in it is the whole point.  A
# dedicated service user would need either its own 97G tree -- and the trees
# the dataset does hold are budgeted for in chromium-build.nix, with no spare
# one in that arithmetic -- or group-write on that one, which does not survive
# the default umask: files one user creates come out group-readable and not
# group-writable, and the next incremental build by the other user fails on
# them.  So the runners are the same user that builds interactively.
#
# The consequence is that anything they execute runs as that user, and that is
# now true of two runners rather than one.  **The compensating control is not
# in this file**: fork pull requests must not be allowed to run on either.
# That is a GitHub repository setting -- Actions -> "Require approval for all
# external contributors" -- and it is per repository rather than per runner,
# so adding the second below did not widen what it has to cover.  Nothing here
# can enforce it, which is exactly why it is written down here.
#
#
# THREE RUNNERS, AND WHAT SEPARATES THEM IS THE CHROMIUM TREE.
#
# This was one runner, deliberately, and the reasoning was that a second
# instance would share /build/chromium with the first -- two concurrent
# `autoninja` runs against one out/ directory corrupt each other, and one
# runner is one job at a time, which is that serialisation for free.
#
# There is more than one tree now, so there are two heavy runners below; the
# light one's argument that follows is unchanged.
#
# The hazard is real and this file still respects it.  But it is an argument
# about the tree rather than about the machine, and the jobs paying for it
# were mostly jobs that never open the tree.  `pinned-engine.yml` is the one
# that paid most: it is `nix build .#engine` -- a fetchurl of a published
# tarball -- plus a compositor build and one pixel guard, and it runs on every
# pull request in that repository because the pair it guards can be broken by
# three different files.
#
# Measured on 2026-09-19, off run #101's JOB timestamps (created 18:21:01,
# started 21:22:45, finished 21:24:36): 1m51s of work behind 3h01m44s of
# queue.  Counted from the run rather than the job it is five minutes longer
# again.
#
# And it is the SPREAD rather than that one number that makes the case.  Two
# more runs of this workflow the same day, both of which really ran: #72
# waited 1m11s for the slot and was finished 3m02s after it was created; #79
# waited 5h55m to do 1m50s of work.  The work either side of that gap is the
# same; only the queue is different.  A job whose cost depends entirely on
# what else is in front of it is not a slow job, it is a queued one, and no
# amount of making it faster would have moved either number.
#
# Deliberately two runs and not a range.  Every earlier draft of this
# paragraph reached for an aggregate -- a count, a floor, a "quickest and
# slowest" -- and every one of them was wrong or went stale, because the day
# was not over and some of the runs had not finished when it was measured.
# A per-run figure off the jobs API is a fact; a min, a max or a count over a
# window that is still moving is a photograph.
#
# (#84 looks faster still at 0.3m and is not a run at all: it was CANCELLED
# seconds after it was created, by #85 entering the same concurrency group,
# and never created a job.  That is the depth-one pending slot working as
# designed -- see scripts/test-engine-concurrency.sh over there -- and it
# would be evidence for the opposite of this paragraph.  Noted because the
# number is in the data and looks like a fast run: anyone re-deriving these
# figures will meet it.)
#
# So: three runners over a pool of trees.  `crux` and `crux-two` both carry
# the `chromium` label and are what every workflow that resets a Chromium tree
# asks for -- two of them, so a repin no longer blocks every other engine
# branch; which tree each run gets is `engine-tree-pool.sh`'s, over in
# cprussin/domicile.  `crux-light` carries no such label and exists for the
# jobs that do not open a tree at all.
#
# NOTHING HERE ENFORCES THAT SPLIT.  A workflow is free to ask for
# `crux-light` and then run `autoninja` in the shared tree, and no NixOS
# option can refuse it.  The backstop is in the other repository --
# .github/scripts/engine-tree-lock.sh, which refuses rather than waits -- and
# it was already the thing that actually kept two writers apart, since the
# single slot said nothing about the person building in that tree by hand.
# What this file now guarantees is narrower and still worth having: a job on
# one runner cannot take the other's slot.
#
# THE GPU IS SHARED, AND ONE GUARD MINDS.  Both runners drive the same render
# node.  That is fine for a pixel comparison, which asks whether a color
# landed, and it is not fine for `guard-latency.sh`, which times sixty
# keystroke-to-pixel rounds and would read a concurrent job as a regression.
#
# **The single slot was that lock, and this file removed it.**  The thing that
# replaces it is `.github/scripts/engine-render-node-lock.sh` in
# cprussin/domicile, and `engine-compile-slot.sh` beside it is the same
# argument about the machine's memory rather than its card.  Both belong over
# there for the reason the tree lock does: they have to be taken by the steps
# that care, and systemd cannot know which those are.
#
# `gpu` IS NOW ON BOTH, so `runs-on: [self-hosted, gpu]` would match either.
# No workflow asks that today.  One that wants a specific machine should name
# `crux` or `crux-light`, which is what both of them are called for.
#
# AND EVERY WORKFLOW THAT ASKS FOR EITHER STILL NEEDS A `concurrency` GROUP
# KEYED BY REF, so queued runs collapse rather than pile up.  That was the
# closing line of this file when there was one runner and it has not stopped
# being true; `scripts/test-engine-concurrency.sh` over there is what enforces
# it, and its glob matches `crux-light` too.
{
  config,
  lib,
  pkgs,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};

  # Kept next to the build tree rather than under /var/lib: a job checks out
  # the repository and then builds the compositor in it with cargo, and that
  # scratch belongs on the NVMe beside the rest of the build rather than on
  # the root pool.  (Cargo and not bun: the jobs that run bun are on
  # `ubuntu-latest`, and neither runner here has ever needed it.)
  #
  # **Nothing in one survives a start.**  The module's root ExecStartPre ends
  # in an unconditional `find -H "$WORK_DIRECTORY" -mindepth 1 -delete` --
  # "always clean workDir" -- so the checkout and its target/ are destroyed on
  # every start, restart and deploy.  Not on every job: these runners are not
  # ephemeral, so the second and later jobs of a session reuse the workspace
  # and only the first after each start pays a cold cargo build.  That split
  # is why the Chromium tree lives in /build/chromium and not in one of these
  # -- they are the disposable half and that one is the whole point.
  #
  # BOTH COUNT AGAINST THE SAME QUOTA the Chromium trees do -- and
  # chromium-build.nix owns that number, with a unit there that refuses a
  # deploy which would not fit -- and each holds its space until the next
  # start rather than until its job ends.  The light one is a checkout and a
  # cargo target/, a few gigabytes beside the tree's 97G and the release
  # build's 40G.  The number that actually has to stay true is the one
  # engine-release.yml checks for itself: 60G free on /build, or it refuses
  # to start.
  workDirs = {
    heavy = "/build/github-runner";
    heavy2 = "/build/github-runner-2";
    light = "/build/github-runner-light";
  };

  # The upstream unit is hardened for a runner that compiles ordinary code.
  # These drive Chromium's own build sandbox and a GPU, and six of those
  # defaults forbid exactly that.  Each is turned off with its reason;
  # everything else the module sets still applies.
  #
  # Shared by both runners rather than written twice.  The light one does not
  # build Chromium, so the bwrap paragraph below is not *its* reason -- but it
  # does enter `nix develop` and drive the same render node, and a second list
  # that differed in some subtle way would be a second thing to keep true for
  # no gain.
  #
  # mkForce only where it is needed, which is not everywhere: the module sets
  # most of these with mkDefault, so a plain value replaces them.  It is
  # required for SystemCallFilter and DeviceAllow, which the module sets with
  # mkBefore so a plain list would *concatenate* rather than replace, and for
  # Restart, which it sets plainly.
  hardening = {
    # THE BUILD SANDBOX.  Chromium is built inside upstream's own nix shell,
    # which is a buildFHSEnv -- and in nixpkgs that is buildFHSEnvBubblewrap,
    # whose shellHook execs bwrap.  bwrap unshares a mount namespace and a
    # user namespace and then mounts inside them, so RestrictNamespaces,
    # ~@mount and PrivateUsers each kill it in the shellHook, before anything
    # it was asked to run.  That is the one thing the heavy runner exists to
    # do.
    RestrictNamespaces = false;
    PrivateUsers = false;
    SystemCallFilter = lib.mkForce [];

    # And the procfs half, which is subtler and was missed the first time.
    # bwrap runs `--proc /proc`, mounting a fresh procfs inside the new user
    # namespace.  The kernel refuses that unless the procfs it inherits is
    # *fully visible*: mount_too_revealing() rejects the mount when locked
    # mounts cover non-empty directories under it, and unsharing a user
    # namespace locks everything inherited.  ProtectKernelTunables read-only
    # binds /proc/sys, /proc/bus, /proc/fs, /proc/irq and /proc/acpi -- all
    # directories -- and that alone is enough for `bwrap: Can't mount proc on
    # /newroot/proc`, in the same shellHook and looking identical to the
    # namespace failure above.
    #
    # ProtectProc is not part of that, though an earlier version of this
    # comment said it was.  mnt_already_visible() compares filesystem type,
    # mnt_root, the locked mount flags and locked children; hidepid is not
    # among them, and a fresh procfs with hidepid=invisible mounts fine.  It
    # is off anyway because it costs nothing and this family has now been
    # wrong twice.
    #
    # ProtectHostname is the least discoverable member of it.  systemd.exec
    # documents it as a UTS namespace and says nothing about /proc, but
    # namespace.c's protect_hostname_yes_table read-only binds
    # /proc/sys/kernel/hostname and /proc/sys/kernel/domainname -- two
    # regular files, so the same shape as /proc/kmsg below, and measured to
    # fail the check on its own.  `"private"` keeps the UTS namespace and
    # skips the table, which is gated on PROTECT_HOSTNAME_YES.
    #
    # ProtectKernelLogs goes for the same reason, and the first version of
    # this comment had it backwards.  It covers /proc/kmsg with an
    # inaccessible node, and mnt_already_visible() rejects a locked child
    # mount over anything that is not a permanently-empty *directory* -- a
    # regular file is not one.  Measured, with bwrap 0.11.2 under a locked
    # /proc/kmsg cover: `Can't mount proc on /newroot/proc: Operation not
    # permitted`, identical to the ProtectKernelTunables failure.  There is
    # no configuration in which one of the two matters and the other does
    # not.
    #
    # ProtectControlGroups stays -- it touches sysfs, and the FHS env's
    # auto-mount loop binds /sys rather than mounting a fresh sysfs, so the
    # visibility check never applies to it.
    #
    # NONE OF THIS BITES TODAY, and that is worth knowing before the next
    # person deletes it.  buildFHSEnvBubblewrap defaults `unsharePid ?
    # false`, so bwrap recursively binds the host /proc rather than mounting
    # a fresh one, and the visibility check is never reached -- which is why
    # the engine build passed on this runner with these still on.  They are
    # off so that the day upstream's shell asks for a pid namespace is not
    # another round of chasing one error message through five directives.
    ProtectProc = "default";
    ProtectKernelTunables = false;
    ProtectHostname = "private";
    # Also uncovers /dev/kmsg and gives back CAP_SYSLOG and syslog(2), all
    # moot: CapabilityBoundingSet is already empty and SystemCallFilter is
    # already cleared.
    ProtectKernelLogs = false;

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
    ProtectClock = false;
    DeviceAllow = lib.mkForce [];

    # /dev/dri's own nodes are group-owned, so these are still needed.
    SupplementaryGroups = ["render" "video"];

    # ProtectSystem=strict with no ReadWritePaths of the module's own, so
    # /build/chromium and /build/tmp would be read-only.  HOME is the
    # workDir, which the module already bind-mounts read-write.
    #
    # /build rather than the one work directory, for both: each runner's
    # TMPDIR is /build/tmp.  The render node lock the companion change adds
    # will have to live under /build too, and for a reason worth writing down
    # here rather than only over there: `PrivateTmp` is per-service, so these
    # two units do NOT share /tmp, and a lock between them cannot live in it.
    ReadWritePaths = ["/build"];

    # A non-ephemeral runner is Restart=no upstream, which is right for a
    # runner whose failure means its token is gone.  Here the likelier cause
    # is a dropped connection to GitHub or a listener that exited on its own,
    # and a unit that stays dead until somebody notices is worse than one
    # that tries again.
    #
    # It deliberately does not cover the /build case.  `Requires=` below
    # cancels the start job when setup-build-mount.service fails, so the
    # service never enters activating and Restart= never applies -- which is
    # what we want, because "/build is not the dataset" is not a condition
    # that fixes itself, and retrying into it every 30s with TMPDIR pointing
    # at RAM is the failure this whole file is written around.
    Restart = lib.mkForce "on-failure";
    RestartSec = 30;

    # The module leaves UMask at 0066, so files a unit writes come out 0600
    # and directories 0711.  Same user either way, so a build still works --
    # but /build/chromium is a tree the runner and the person share, and half
    # of it being unreadable to everything else is a surprise waiting for the
    # first `sudo` or the first `rsync`.  0022 is what an interactive build
    # already writes.
    #
    # It loosens the work directories too, which is where a checkout's
    # persisted git credentials live -- hence the 0750 on both of them below.
    # The state directories are unaffected: StateDirectoryMode is 0700 and the
    # runners' token files are written with explicit modes by an ExecStartPre
    # that ignores the umask.
    UMask = "0022";

    # RemoveIPC deletes every System V and POSIX IPC object owned by the
    # unit's user when it stops.  That is right for a service user who owns
    # nothing else and wrong here, where the whole premise of this file is
    # that the same person also builds and works interactively: a restart
    # would take out IPC belonging to their live ssh sessions, and
    # Restart=on-failure means it fires on a loop rather than once.
    #
    # It matters more with two units than it did with one: a deploy restarts
    # both, so the window in which this would fire is now twice as wide.
    RemoveIPC = false;
  };

  # One runner.  `name` is what identifies it in the repository's settings and
  # `labels` is what `runs-on` actually matches -- they are spelled alike on
  # purpose, so that what a workflow asks for and what shows up in that list
  # read the same.
  runner = {
    name,
    workDir,
    labels,
  }: {
    enable = true;
    url = "https://github.com/cprussin/domicile";
    tokenFile = config.deployment.keys.domicile-github-runner-token.path;

    inherit name workDir;
    extraLabels = labels;

    # Not ephemeral.  An ephemeral runner de-registers after every job, which
    # is the right posture for a runner that starts from nothing -- and these
    # exist precisely because they do not: the heavy one has the Chromium
    # tree, and the light one keeps a cargo target/ between the jobs of a
    # session.
    #
    # How much that is worth is not measured.  The 39s `pinned-engine.yml`
    # records for its compositor build is a warm nix store and a COLD target/
    # -- the module's root ExecStartPre empties the work directory on every
    # start, so the first job after each start pays that -- and the warm-target
    # number would be lower.  39s is the ceiling this buys against, not the
    # saving.
    ephemeral = false;
    replace = true;

    user = config.primary-user.name;

    # `nix` is what the engine workflow enters Chromium's own toolchain shell
    # with (`nix-shell "$CHROMIUM/tools/nix/shell.nix"` -- NOT `nix develop`,
    # which that workflow's own comment records trying: upstream ships a
    # shell.nix rather than a flake, and the develop form entered nothing and
    # exited 0); the rest are what depot_tools and the repository's own checks
    # reach for.  A service's PATH is not a login shell's, so anything a
    # workflow runs by bare name has to be here.
    # Only what the module does not already put on PATH -- it supplies bash,
    # coreutils, git, tar, gzip and nix itself, and naming them again just
    # puts them there twice.  `cacert` would be worse than redundant: it is a
    # package with no binaries, so it adds a PATH entry and no trust, and TLS
    # trust here comes from /etc/ssl.
    #
    # One list for both.  The light runner needs none of depot_tools' probes,
    # but a PATH entry it never reads costs nothing and a second list that
    # drifted from this one would cost a session.
    extraPackages = with pkgs; [
      curl
      gawk # `awk` in a `run:` step is otherwise not there
      jq
      lsb-release # depot_tools probes it
      python3
      which
      xz
      zstd # actions/cache silently falls back to gzip without it
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

    serviceOverrides = hardening;
  };

  # What every one of these units needs before it can start, applied to all
  # three.  The heavy ones add the tree pool to it below; see the comment
  # there.
  wiring = {
    # Colmena writes the token asynchronously and generates a -key.service as
    # the gate on it; every other key consumer in this repository pairs both,
    # and these have to as well.  Without it a start before the key lands
    # fails on `install ... ${tokenFile}` and, since a non-ephemeral runner is
    # Restart=no by default, never comes back.
    requires = [
      "domicile-github-runner-token-key.service"

      # Not just `after`.  setup-build-mount.service's failure case is "/build
      # is mounted but it is not the dataset", and ordering alone would let a
      # runner start anyway with TMPDIR pointing at whatever is there -- which
      # is the exact case that check was written to catch.
      "setup-build-mount.service"
    ];

    # /build is a nofail mount, so nothing waits for it on its own, and
    # setup-build-mount.service is what checks that what is mounted there is
    # actually the dataset.  chromium-build.nix wrote that check because
    # TMPDIR pointing at a tmpfs root is what takes this machine out -- and
    # these units set exactly that TMPDIR.
    after = ["domicile-github-runner-token-key.service" "setup-build-mount.service"];
    unitConfig.RequiresMountsFor = "/build";
  };
in {
  deployment.keys.domicile-github-runner-token = {
    # A personal access token rather than a runner registration token: the
    # registration kind expires in an hour, so a rebuild any later than that
    # would fail to re-register and the runner would simply stop existing.  A
    # PAT lets each service mint its own registration token whenever it
    # starts.
    #
    # One key for both runners.  A registration token is minted per start
    # rather than held, so two units reading this file race over nothing.
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
  # Without these the units fail twice on a first deploy and stay failed: the
  # root ExecStartPre runs `find -H "$WORK_DIRECTORY" -mindepth 1 -delete`
  # under `set -euo pipefail`, and BindPaths names the directory unprefixed,
  # which systemd treats as fatal when the source is missing.
  # 0750 rather than 0755, and it is paired with the UMask above.  Relaxing
  # the unit's umask to 0022 makes everything a runner writes here readable
  # to everyone -- and actions/checkout persists credentials by default, so
  # _work/<repo>/.git/config carries a live token header for the length of a
  # job.  The umask change is wanted for /build/chromium, which is shared;
  # these directories are not, so the directory bit takes back what it gave
  # away.
  systemd.tmpfiles.rules = [
    "d ${workDirs.heavy} 0750 ${config.primary-user.name} users -"
    "d ${workDirs.heavy2} 0750 ${config.primary-user.name} users -"
    "d ${workDirs.light} 0750 ${config.primary-user.name} users -"
  ];

  systemd.services = {
    # THE HEAVY ONES ALSO WAIT FOR THE TREE POOL, AND THE LIGHT ONE MUST NOT.
    #
    # setup-chromium-trees.service is what makes /build/chromium a symlink into
    # /build/trees, including the one-time move of the 97G checkout into the
    # first slot.  A heavy runner that started before it finished would hand a
    # job a path that is still a directory -- which engine-tree-pool.sh refuses
    # by design -- and would be the window in which a job is inside the tree
    # while the move wants to happen, which is what that unit's lock check is
    # about.  `Requires=` and not just `After=`, for setup-build-mount's
    # reason: a half-made pool is not a thing to start a job into.
    #
    # The light runner is the whole argument of this file: it exists BECAUSE it
    # never opens that tree.  Making it require the pool would mean a refusal
    # up there -- a stale lock, which by design nothing clears automatically --
    # takes out all of CI rather than the half that touches Chromium, and the
    # jobs it would take out are the ones that were already waiting hours
    # behind the tree for no part of it.
    github-runner-domicile =
      wiring
      // {
        requires = wiring.requires ++ ["setup-chromium-trees.service"];
        after = wiring.after ++ ["setup-chromium-trees.service"];
      };
    github-runner-domicile-two =
      wiring
      // {
        requires = wiring.requires ++ ["setup-chromium-trees.service"];
        after = wiring.after ++ ["setup-chromium-trees.service"];
      };
    github-runner-domicile-light = wiring;
  };

  services.github-runners = {
    # The tree, and everything that resets it.  `chromium` is the label that
    # says so, and `engine.yml`, `engine-release.yml` and
    # `engine-drm-probe.yml` ask for `crux` by name.  So does
    # `pinned-engine.yml` today, which is the thing the companion change moves
    # and the whole reason the runner below exists.
    domicile = runner {
      name = "crux";
      workDir = workDirs.heavy;
      labels = ["crux" "chromium" "gpu"];
    };

    # Same labels: the pool (`engine-tree-pool.sh`) arbitrates trees, and
    # `engine-compile-slot.sh` keeps cold builds to one at a time (62G, no
    # swap).  Both live in cprussin/domicile.
    domicile-two = runner {
      name = "crux-two";
      workDir = workDirs.heavy2;
      labels = ["crux" "chromium" "gpu"];
    };

    # Everything that does not.  One job uses it, `pinned-engine.yml`, and it
    # is worth a whole registration of its own: it fetches the
    # published engine rather than building one, it runs on every pull request
    # because the pair it guards can be broken by three different files, it is
    # under two minutes of work, and it was spending hours in front of the tree
    # for no part of the tree.
    domicile-light = runner {
      name = "crux-light";
      workDir = workDirs.light;
      labels = ["crux-light" "gpu"];
    };
  };
}
