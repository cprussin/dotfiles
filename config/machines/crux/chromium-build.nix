# Building Chromium here means going through upstream's own Nix shell, which is
# what supplies pkg-config and the other host tools gn probes for:
#
#     cd /build/chromium/src
#     nix develop "path:./tools/nix"
#
# The path: prefix matters -- without it the flake ref resolves to the
# enclosing git repo and nix copies Chromium's tracked tree into the store
# first.  Gitignored paths are excluded, which is the only reason that is
# merely slow rather than catastrophic: out/ is most of the checkout.
#
# --command does not work here, and not for the reason it looks like: nix
# passes everything after that flag through untouched, but upstream's shell
# is a buildFHSEnv whose shellHook execs bwrap before nix's appended `exec
# <command>` is ever reached, so you land in an interactive bash having run
# nothing.  Upstream ships an escape hatch for scripting it:
#
#     NIX_SHELL_RUN='autoninja -C out/Domicile chrome' nix develop "path:./tools/nix"
#
# TMPDIR is nested rather than preserved: nix develop drops the derivation's
# value, then exports a fresh mktemp directory created under the caller's, so
# the shell sees /build/tmp/nix-shell.XXXXXX.  bwrap leaves /build visible at
# the same path, so the build's temp files still land on the dataset rather
# than tmpfs, which is the point.  Those per-shell directories are not removed
# on exit -- that is what the age on the tmpfiles rule below is for.
#
# Measured 2026-08-29, first clean build, is_component_build with
# symbol_level=0: 4h16m wall on 16 jobs, 97G of the 200G quota.  Both would
# grow substantially with symbols or a second out/ configuration.
{
  config,
  pkgs,
  utils,
  ...
}: let
  zfs = pkgs.callPackage ../../../lib/zfs.nix {};

  # tank-fast is the NVMe and much the better disk for ninja, but it is also
  # the 500G that holds /nix, so the dataset is created with `-o quota=200G`:
  # an overrun then fails the build rather than the machine.  Moving to tank is
  # not a one-line change here -- tank is LUKS with detached keys and is
  # imported by import-tank.service, so its datasets are mounted by that unit
  # rather than through fileSystems, and run-backup's `zfs send -R` would
  # replicate the tree to the external disk unless it is added to that
  # command's -X list.
  pool = "tank-fast";

  buildRoot = "/build";

  # HOW MANY CHROMIUM TREES THIS MACHINE KEEPS, and the number is a disk
  # decision rather than a CI one -- which is why it is here and not in
  # cprussin/domicile.
  #
  # There was one tree, so the pin it carried was whichever branch ran last.
  # `packages/domicile-engine/CHROMIUM_PIN` is what out/Domicile is compiled
  # against, so moving it invalidates the whole of that directory -- and
  # invalidates it in BOTH directions, because going back to the older pin is
  # as much of a rebuild as going forward was.  Two pull requests on different
  # pins do not interleave in one tree; they take turns, and every turn is
  # 4h05m (measured, run 35576710600).  With a pull-request run and a merge run
  # each, two open branches is four of those, on the one job slot everything
  # else over there queues behind.
  #
  # Two trees is the whole of the fix for that case: the repin builds in one,
  # everything still on the old pin keeps the other, and neither evicts the
  # other.  `engine-tree-pool.sh` picks between them and asks for no particular
  # number -- it uses whatever slots it finds here -- so this can be raised
  # later without touching that repository.
  #
  # THE ARITHMETIC, because it is what stops at two.  A tree is 97G (the
  # measurement above), `engine-release.yml`'s out/Release is ~40G in whichever
  # tree it lands in, and the two runner work directories and /build/tmp are a
  # few more.  Two trees is 194 + 40 = 234G before those, so the dataset's
  # quota goes from 200G to **250G**, which is the next round number with room
  # left over.  The unit below checks the 234 and asks for the 250.
  #
  # THAT QUOTA IS NOT SET HERE, AND THE UNIT BELOW REFUSES UNTIL IT IS.  The
  # dataset was created by hand with `-o quota=200G` and nothing in this file
  # declares it, so raising it is a command on the machine:
  #
  #     zfs set quota=250G tank-fast/chromium
  #
  # Spelled literally rather than with ${pool}, because this is a comment: Nix
  # does not interpolate one, so in a shell that name expands to nothing and
  # the command silently addresses `/chromium`.
  #
  # CHECKED RATHER THAN ASSUMED, because every other prerequisite in this file
  # is.  Deployed against the live 200G, the pool looks fine and fails much
  # later and much worse: the second tree fills until EDQUOT partway through a
  # 4h16m build, and `engine-release.yml`'s "60G free on /build" check can
  # never pass again.  A deploy that stops with the command to run is the
  # cheap version of finding that out.
  #
  # Three trees would be 291 + 40 on a 500G NVMe that also holds /nix, which is
  # why it is not three.  A third is worth having only once something has
  # measured what is actually left on that disk.
  #
  # AT LEAST ONE, and it is a literal here rather than an option, so this is a
  # note and not an assertion: the unit below adopts the existing checkout into
  # `tree-0` and points the link at it, so a zero would reach an `rmdir` and an
  # `ln -s` against a slot nothing made.
  treeCount = 2;
in {
  # /build rather than /home because this is machine-scoped regenerable
  # scratch, not user state: the /home datasets on this fleet are persisted and
  # backed up, and on crux one would land on tank with everything the comment
  # above describes.
  #
  # nofail so that a missing dataset cannot fail local-fs.target, which would
  # leave crux in emergency.target with no sshd -- recoverable only at the
  # physical console, on the box that serves the house's DNS.  nofail also
  # drops the mount's implicit Before=local-fs.target (and its
  # After=local-fs-pre.target, which nothing here needs), so x-systemd.before
  # puts the ordering the tmpfiles rules depend on back, without making the
  # mount required again.
  fileSystems = zfs.mkZfsFileSystems {
    "${pool}/chromium" = {
      mountpoint = buildRoot;
      options = ["defaults" "nofail" "x-systemd.before=local-fs.target"];
    };
  };

  systemd = {
    tmpfiles.rules = [
      "d ${buildRoot} 0755 ${config.primary-user.name} users -"

      # /tmp is tmpfs, so it is RAM; a Chromium link there takes the machine
      # out.  This one is on disk and so does not empty on reboot the way /tmp
      # does, hence the age.
      "d ${buildRoot}/tmp 1777 root root 10d"
    ];

    # x-systemd.before fixes the boot ordering, but not the deploy: on the
    # switch that introduces ${buildRoot}, switch-to-configuration restarts
    # sysinit-reactivation.target (which re-runs systemd-tmpfiles) and blocks
    # on it before starting new mounts, so the rules above land on the tmpfs
    # root and the mount then hides them.  Redoing them here, after the mount,
    # is what makes the dataset root come out owned by the primary user with
    # its tmp directory present.  A failed build.mount already degrades the
    # system on its own; what this adds is that repair, plus catching the
    # cases the mount unit cannot report -- masked or stopped, unmounted after
    # boot, or something other than the dataset mounted over it.
    #
    # Deliberately no RemainAfterExit: a oneshot that goes back to inactive is
    # pulled in afresh by every switch's multi-user.target job, which is how
    # this runs on each deploy rather than only the first.  It does mean a
    # healthy unit reads "inactive (dead)" rather than "active (exited)", so
    # check ${buildRoot} itself rather than the unit's state.
    services.setup-build-mount = {
      description = "Set ${buildRoot} up on the ${pool}/chromium dataset";
      wantedBy = ["multi-user.target"];
      after = ["${utils.escapeSystemdPath buildRoot}.mount" "local-fs.target"];
      serviceConfig = {
        Type = "oneshot";

        # systemd-tmpfiles exits 65/73 when any file under /etc/tmpfiles.d is
        # bad, whether or not it is one of ours.  Upstream's own tmpfiles units
        # treat both as success; failing here would fail every deploy to crux.
        SuccessExitStatus = "DATAERR CANTCREAT";
        ExecStart = pkgs.writeShellScript "setup-build-mount" ''
          set -eu
          mounted=$(${pkgs.util-linux}/bin/findmnt --noheadings --output SOURCE --mountpoint ${buildRoot} || true)
          if [ "$mounted" != "${pool}/chromium" ]; then
            echo "${buildRoot} is not ${pool}/chromium (found: ''${mounted:-nothing})." >&2
            echo "TMPDIR points into it, so anything using it would write to RAM." >&2
            exit 1
          fi
          exec ${config.systemd.package}/bin/systemd-tmpfiles --create --prefix=${buildRoot}
        '';
      };
    };

    # THE SHAPE THE TREE POOL NEEDS, and nothing else.
    #
    # `${buildRoot}/chromium` is a SYMLINK into ${buildRoot}/trees, and
    # everything -- CI, this file's own instructions, a person at a terminal --
    # keeps saying ${buildRoot}/chromium/src.  That indirection is the whole
    # mechanism: `gn gen` bakes absolute paths into out/Domicile, so a tree
    # reached at a different path is a tree whose every compile command
    # changed, which is a clean build with extra steps.  Behind one symlink
    # every tree is built and read at the same path and swapping between them
    # costs nothing.
    #
    # WHICH tree the symlink names at any moment is NOT this unit's business.
    # That is `.github/scripts/engine-tree-pool.sh` in cprussin/domicile,
    # chosen per run from the pin the run carries, under the tree lock.  This
    # unit only guarantees the slots exist and that the path is a symlink
    # rather than a tree, because both of those are facts about /build and
    # /build is this file's.
    #
    # THE ADOPTION IS THE PART THAT HAPPENS ONCE.  Before the pool,
    # ${buildRoot}/chromium is a real directory holding the only Chromium
    # checkout on this machine -- 97G of it -- and it has to become the first
    # slot.  A `mv` within the dataset is a rename and so is instant, and it is
    # the one thing here that is not repeatable, which is why it refuses in
    # every case it is not certain about rather than choosing.
    #
    # It refuses to make that move while the tree lock is held.  A deploy does
    # not wait for CI, and renaming the checkout out from under a running
    # `autoninja` is the silent corruption that lock exists for -- siso carries
    # on and links a binary from two trees.  Failing the unit is the loud
    # version of that, and the runners in domicile-ci.nix require this one, so
    # they do not start into a half-made pool.
    services.setup-chromium-trees = {
      description = "Chromium build trees under ${buildRoot}";
      wantedBy = ["multi-user.target"];
      after = ["setup-build-mount.service"];
      requires = ["setup-build-mount.service"];
      unitConfig.RequiresMountsFor = buildRoot;
      # A service's PATH is not a login shell's, and every command below is a
      # bare name.  setup-build-mount.service reaches for absolute store paths
      # instead; this one needs half of coreutils, so the list is shorter.
      # `zfs` is for the quota check, which is the one thing here that asks
      # the pool a question rather than the filesystem.
      path = [pkgs.coreutils config.boot.zfs.package];
      serviceConfig = {
        Type = "oneshot";

        # No RemainAfterExit, for setup-build-mount.service's reason: a
        # oneshot that goes back to inactive is pulled in afresh by every
        # switch's multi-user.target job, which is how RAISING treeCount above
        # takes effect on a deploy rather than on the next reboot.  Lowering it
        # does not: this only ever adds slots, and a slot it stopped making is
        # still a directory that engine-tree-pool.sh will find and hand out.
        # Removing one is a `rm -rf` of 97G and belongs to a person.  A
        # healthy unit therefore reads "inactive (dead)"; check ${buildRoot}
        # itself rather than the unit's state.
        ExecStart = pkgs.writeShellScript "setup-chromium-trees" ''
          set -eu
          trees=${buildRoot}/trees
          link=${buildRoot}/chromium

          # ROOM FOR THE TREES THIS UNIT IS ABOUT TO MAKE.  Before anything is
          # created, which also means before the lock check below -- so the
          # first deploy at the old quota stops here, and the stale lock that
          # same deploy's runner restart may have left is met on the NEXT one.
          # Two round trips, each naming its own command, and this is the
          # order that has to hold: nothing should be made in a dataset that
          # cannot hold it.
          #
          # A tree is 97G and engine-release.yml's out/Release is ~40G in
          # whichever one it lands in, so the FLOOR is treeCount of the first
          # plus one of the second.
          #
          # The floor is what is checked and not what is recommended, and the
          # gap is deliberate: it counts the trees and nothing else, while the
          # dataset also carries the two runner work directories and
          # /build/tmp.  A quota set exactly at the floor passes here and then
          # runs out somewhere less legible.  So the command below asks for
          # the next round number above it.
          #
          # `0` IS WHAT A DATASET WITH NO QUOTA PRINTS, and it is worth
          # writing down because the un-`-p` form prints `none` instead: in
          # libzfs_dataset.c's ZFS_PROP_QUOTA branch, a zero value is "0" when
          # `literal` is set and "none" when it is not.  `-` is only ever
          # printed for a property that does not apply to the dataset TYPE --
          # zvols, snapshots, bookmarks -- and this is a filesystem.  So the
          # two cases here are "0" and a decimal byte count, and a quota-less
          # dataset cannot be refused forever.
          #
          # A `zfs` that fails outright is a failure rather than a pass: under
          # `set -e` a BARE assignment from a command substitution does abort,
          # so a renamed dataset or an unimported pool stops the unit here.
          # Bare is the load-bearing word, and it is the PREFIX that decides
          # rather than the scope: `local`, `export` and `declare` each
          # swallow the substitution's status, so `export quota=$(zfs ...)`
          # would not stop anything even here at the top level, while a bare
          # assignment inside a function still would.  This one is bare.
          floor=$(( ${toString treeCount} * 97 + 40 ))
          want=$(( (floor / 50 + 1) * 50 ))
          quota=$(zfs get -Hp -o value quota ${pool}/chromium)
          if [ "$quota" != "0" ] && [ "$quota" -lt $((floor * 1024 * 1024 * 1024)) ]; then
            echo "${pool}/chromium has a $((quota / 1024 / 1024 / 1024))G quota." >&2
            echo "${toString treeCount} Chromium trees plus a release build need ''${floor}G of it," >&2
            echo "before the runner work directories and /build/tmp." >&2
            echo >&2
            echo "  zfs set quota=''${want}G ${pool}/chromium" >&2
            echo >&2
            echo "Checked here rather than left to the build, because the way" >&2
            echo "a short quota shows up otherwise is EDQUOT partway through a" >&2
            echo "four-hour Chromium build." >&2
            exit 1
          fi

          mkdir -p "$trees"
          for slot in $(seq 0 ${toString (treeCount - 1)}); do
            mkdir -p "$trees/tree-$slot"
          done

          # THE ONE-TIME MOVE.  Only from a real directory, only into an empty
          # first slot: a tree-0 with something in it means this already
          # happened and something else put a directory back at $link, which
          # is a thing to look at rather than a thing to merge.
          if [ -e "$link" ] && [ ! -L "$link" ] && [ ! -d "$link" ]; then
            # Not a directory and not a symlink.  There is no reading of that
            # which this unit should act on: adopting it would put a stray
            # file where the only Chromium checkout belongs, and leaving it
            # would mean exiting 0 over a path no build can use.
            echo "$link is neither a symlink nor a directory." >&2
            echo "Whatever it is, it is in the place the Chromium tree goes." >&2
            echo "Move or remove it by hand." >&2
            exit 1
          fi

          if [ -d "$link" ] && [ ! -L "$link" ]; then
            # The lock is checked HERE rather than at the top, and that is
            # deliberate.  This unit runs on every deploy, and a deploy does
            # not wait for CI -- so a check up there would fail the deploy
            # any time a job happened to be building, for a move that on all
            # but one deploy in the machine's life is not going to happen.
            # Inside the branch it guards exactly the operation that is
            # unsafe: renaming the checkout out from under a running
            # `autoninja` is the silent corruption that lock exists for, and
            # siso would carry on and link a binary from two trees.
            # BOTH PLACES THE LOCK CAN BE, and that is not belt and braces.
            # cprussin/domicile moves it to the build root in the same change
            # that adds the pool -- because a lock beside a checkout that is
            # now a symlink would be written inside whichever tree the link
            # named.  But the two repositories land in either order, and until
            # that one does, a running engine job's lock is at
            # $link/.domicile-tree-lock, which is where a person following
            # engine-tree-lock.sh's own header would have taken it by hand.
            # Checking only the new path would mean the guard never fires on
            # exactly the deploy it exists for: the one where the adoption
            # actually runs.
            # Two operations, and a race between them is accepted rather
            # than closed: this tests, and the `mv` below acts.  The window
            # is two tests, an `ls` and an `rmdir` wide, and the move itself
            # is an intra-dataset rename.  Closing it would mean this unit
            # taking BOTH candidate locks in engine-tree-lock.sh's own format
            # and dropping them from a trap -- which a unit killed mid-deploy
            # would fail to do, leaving exactly the stale lock the message
            # below exists to explain.
            held=""
            for candidate in "${buildRoot}/.domicile-tree-lock" "$link/.domicile-tree-lock"; do
              [ -e "$candidate" ] || continue
              held="$candidate"
              break
            done
            if [ -n "$held" ]; then
              # AND THE STALE CASE, WHICH THIS DEPLOY IS THE LIKELIEST CAUSE
              # OF.  Switching the configuration restarts the heavy runner, so
              # an engine job in flight is killed -- and a killed job never
              # reaches its `if: always()` step, so the lock it took stays.
              # This unit then refuses, `Requires=` in domicile-ci.nix keeps
              # the runner down, and the next deploy refuses for the same
              # reason: "wait for that job to finish" is advice about a job
              # that no longer exists, and nothing clears the lock on its own
              # (engine-tree-lock.sh's own header says why a timeout must
              # not).  So say both things, and name the one command.
              echo "$held is held, so something may be building in $link and" >&2
              echo "moving it is not safe." >&2
              echo >&2
              echo "  taken by: $(cat "$held/owner" 2>/dev/null || echo 'someone who did not write their name in it')" >&2
              echo "  taken at: $(cat "$held/since-human" 2>/dev/null || echo 'an unrecorded time')" >&2
              echo >&2
              echo "If that build is still running, wait for it -- a Chromium" >&2
              echo "build is up to four hours -- and deploy again." >&2
              echo >&2
              echo "If it is not, the lock is stale, and this deploy is the" >&2
              echo "likeliest reason: restarting the runner kills a job before" >&2
              echo "it can drop its lock.  Nothing clears it automatically." >&2
              echo "This does:" >&2
              echo >&2
              echo "  rm -rf $held" >&2
              exit 1
            fi
            if [ -n "$(ls -A "$trees/tree-0" 2>/dev/null)" ]; then
              echo "$link is a directory and $trees/tree-0 is not empty." >&2
              echo "Both hold a Chromium checkout and this will not choose" >&2
              echo "between them.  Move or remove one by hand." >&2
              exit 1
            fi
            echo "adopting $link as $trees/tree-0"
            rmdir "$trees/tree-0"
            mv -T "$link" "$trees/tree-0"
          fi

          # A machine that has never had a checkout gets an empty first slot,
          # and the first engine run pays the 4h16m clean build into it.
          #
          # A LINK THAT POINTS NOWHERE IS REPOINTED RATHER THAN LEFT, and the
          # distinction is why `-L` is tested separately from `-e`: a symlink
          # whose target is gone answers `-L` and not `-e`.  Left alone it
          # would survive every later run of this unit -- `ls -ld` below
          # succeeds on it -- so the unit would exit 0 reporting health over a
          # path that reaches nothing, and the failure would land on the next
          # engine job instead.  A slot removed by hand, or `treeCount`
          # lowered past the slot the link named, is how it gets there.
          if [ -L "$link" ] && [ ! -e "$link" ]; then
            echo "$link pointed at $(readlink "$link"), which is gone; repointing it"
            ln -sfn "$trees/tree-0" "$link"
          elif [ ! -e "$link" ]; then
            ln -s "$trees/tree-0" "$link"
          fi

          # The slot directories and the link, NOT what is inside them.  A
          # tree holds several million files and a recursive chown over it on
          # every deploy is minutes of walking a checkout whose ownership was
          # already right -- the runners and the person are the same user, and
          # an adopted tree keeps what it had.
          chown ${config.primary-user.name}:users "$trees" "$trees"/*
          chown -h ${config.primary-user.name}:users "$link"
          chmod 0755 "$trees" "$trees"/*

          ls -ld "$link" "$trees"/*
        '';
      };
    };
  };

  # Chromium's toolchain is prebuilt dynamically-linked binaries fetched by
  # gclient hooks, which cannot run against the store unaided.  If one dies on
  # a missing .so, add it to programs.nix-ld.libraries -- systemPackages will
  # not help.
  #
  # It is not consulted inside upstream's shell: that is a buildFHSEnv, so the
  # loader at /lib64 in the container is the FHS env's own rather than nix-ld's
  # stub.  Whether anything still needs it is untested -- the first `fetch
  # chromium` is necessarily driven from outside that shell, so that is where
  # it would earn its place.  Testing costs a switch and a second rather than a
  # rebuild: turn this off and run a fetched binary directly, e.g.
  # third_party/llvm-build/Release+Asserts/bin/clang --version.
  programs.nix-ld.enable = true;

  environment = {
    systemPackages = [
      pkgs.curl
      pkgs.git
      pkgs.lsb-release # depot_tools probes it
      pkgs.python3
    ];

    # sessionVariables rather than variables: the primary user's shell is
    # nushell, which sources neither /etc/profile nor home-manager's
    # hm-session-vars.sh, so the host-wide option is the only one that reaches
    # the shell that needs it.  This covers every PAM session, including
    # `ssh crux <cmd>`, but no system service.
    sessionVariables.TMPDIR = "${buildRoot}/tmp";
  };
}
