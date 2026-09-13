# What the Domicile tty work needs from this machine.
#
# Domicile carries a Chromium fork -- chromium-build.nix owns its tree here and
# domicile-ci.nix the runner that builds it.  The step this file serves is
# getting Chromium's ozone/drm platform to light a screen on a bare tty, with
# no display server under it at all (docs/architecture/A-DESKTOP-ON-A-TTY.md in
# the domicile repo).  That needs two things this machine does not otherwise
# have: a DRM card node the primary user can open, and a connector that is
# actually connected.
#
# It is deliberately separate from domicile-ci.nix and claude-agent.nix even
# though it serves both.  Those two files are each about *a way of working on
# the engine* -- a runner, an interactive agent -- and this is about the
# hardware underneath them, which either one uses and neither one owns.
#
#
# WHAT IS ACTUALLY WRONG WITH THE CARD, WHICH IS NOT WHAT IT LOOKS LIKE.
#
# The project doc says "crux has no card node".  The node is there: GTX 970,
# `nvidia-drm` with KMS on (hardware.nix sets modesetting.enable, so
# `fbdev=1 modeset=1` is already correct and wants no change here), four CRTCs,
# atomic accepted, GBM working.  /dev/dri/card0 is root:video 0660 and the
# `video` group has no members, so no *ssh* session can open it.  That is item
# 1 below, and it is one line.
#
# "ssh" and not "login", because two other things could already open it and
# neither is what the work runs as:
#
#   - The CI runner.  domicile-ci.nix gives its unit `SupplementaryGroups =
#     ["render" "video"]`; systemd resolves that group *name* through NSS but
#     not its member list, so the unit gets the gid whether or not anybody is
#     in the group.  That service has been able to open the card since it
#     landed.
#   - A session at the physical console, through logind's uaccess ACL -- see
#     the note further down, which is also why `hide` does not survive one.
#
# So what item 1 adds is interactive ssh and claude-agent.nix, which is where
# the work is actually driven from.
#
# The connectors are the real problem, and no permission fixes them: all four
# read `disconnected` because the machine is headless in a closet.  Forcing one
# on with `video=HDMI-A-1:1920x1080e` on the kernel command line is the usual
# trick and does not work here -- the proprietary nvidia driver does not honour
# the standard `video=` parameters.  Item 2 is the answer instead.
{
  config,
  lib,
  pkgs,
  ...
}: let
  user = config.primary-user.name;

  # A wrapper rather than the `sudo setfacl` this was first asked for.  Both
  # give the same power to the same person; the difference is what a *bug or a
  # careless argument* can reach.  `setfacl` as root takes any path, so a typo
  # rewrites the ACL on anything on the box.  This takes a bare node name that
  # cannot contain a `/` or a `.` and builds the path itself, so /dev/dri is
  # the whole of its reach, and it can only ever name one user -- the one
  # below, resolved at build time, not an argument.
  drm-acl = pkgs.writeShellScriptBin "drm-acl" ''
    set -eu

    user=${lib.escapeShellArg user}

    usage() {
      echo "usage: drm-acl status          # what each node is, and whether it is hidden" >&2
      echo "       sudo drm-acl hide WHICH # deny $user, so Chromium skips the node" >&2
      echo "       sudo drm-acl show WHICH # undo" >&2
      echo "" >&2
      echo "WHICH is either a driver -- nvidia-drm, vkms -- or a bare node name" >&2
      echo "under /dev/dri such as card0 or renderD128.  Prefer the driver: cardN" >&2
      echo "numbering is registration order and is not promised to hold." >&2
      echo "" >&2
      echo "A driver resolves to that driver's *card* node only, which is what" >&2
      echo "Chromium selects on; name a render node explicitly to touch one." >&2
      exit 2
    }

    # The driver bound to a node, which is the only stable name it has.  cardN
    # numbering is registration order -- nvidia-drm comes up with the rest of
    # the graphics stack and vkms from systemd-modules-load, so nvidia is
    # card0 today, but nothing enforces that and hiding the wrong one is a
    # confusing failure rather than a loud one.  Hence `status`, and hence
    # reading this rather than assuming.
    driver() {
      link=/sys/class/drm/$1/device/driver
      if [ -e "$link" ]
      then ${pkgs.coreutils}/bin/basename "$(${pkgs.coreutils}/bin/readlink -f "$link")"
      else echo "-"
      fi
    }

    hidden() {
      ${pkgs.acl}/bin/getfacl -cE -- "$1" 2>/dev/null \
        | ${pkgs.gnugrep}/bin/grep -qxF "user:$user:---"
    }

    # Takes either spelling and lands on one node.  The driver spelling is the
    # one to use and the reason is the `driver()` comment above: naming `card0`
    # is naming a slot in registration order, and the day that order changes,
    # `hide card0` hides the wrong card and the test quietly runs against a
    # blind GPU.  Naming `nvidia-drm` cannot do that.
    #
    # Sole guard on a script that runs as root out of a NOPASSWD rule, so it
    # rejects rather than sanitises.  Both spellings are matched against a
    # closed set before anything is built from them: a node name must be
    # alphanumeric with a `card`/`renderD` prefix, and a driver name is only
    # ever compared against what sysfs already says, never used to build a
    # path.  Nothing the caller types reaches `setfacl` except as a
    # /dev/dri/<validated name> that was confirmed to be a character device.
    resolve() {
      case "$1" in
        card*|renderD*)
          case "$1" in
            *[!a-zA-Z0-9]*)
              echo "drm-acl: not a DRM node name: $1" >&2
              exit 1
              ;;
          esac
          node=/dev/dri/$1
          [ -c "$node" ] || { echo "drm-acl: no such device: $node" >&2; exit 1; }
          return
          ;;
      esac

      # `driver()` prints `-` for a node with no bound driver, so refuse it
      # here rather than let it match one.  Nobody means "the driverless card".
      [ "$1" != "-" ] || { echo "drm-acl: not a driver: -" >&2; exit 1; }

      # Not a node name, so treat it as a driver and let sysfs resolve it.
      # Refuses on more than one match rather than picking: two cards on one
      # driver is not a case this box has, and guessing which was meant is how
      # the wrong screen gets blanked.
      node=
      for path in /dev/dri/card*
      do
        [ -c "$path" ] || continue
        name=$(${pkgs.coreutils}/bin/basename "$path")
        [ "$(driver "$name")" = "$1" ] || continue
        if [ -n "$node" ]
        then
          echo "drm-acl: $1 drives more than one node; name one of them." >&2
          exit 1
        fi
        node=$path
      done
      [ -n "$node" ] || {
        echo "drm-acl: no card node and no driver called: $1" >&2
        echo "  \`drm-acl status\` lists what is here." >&2
        exit 1
      }
    }

    case "''${1-}" in
      status)
        [ $# -eq 1 ] || usage
        for path in /dev/dri/card* /dev/dri/renderD*
        do
          [ -c "$path" ] || continue
          name=$(${pkgs.coreutils}/bin/basename "$path")
          # "not hidden" rather than "openable": this looks for the deny
          # entry and nothing else, so it does not speak to mode or group.
          if hidden "$path"
          then state="hidden from $user"
          else state="not hidden from $user"
          fi
          ${pkgs.coreutils}/bin/printf '%-12s %-12s %s\n' "$name" "$(driver "$name")" "$state"
        done
        ;;

      hide)
        [ $# -eq 2 ] || usage
        resolve "$2"
        # A named-user entry beats the group entry, so this denies the user
        # even though item 1 puts them in `video`.  That is the point: group
        # membership is the floor and this is the switch on top of it.
        ${pkgs.acl}/bin/setfacl -m "u:$user:---" -- "$node"
        ;;

      show)
        [ $# -eq 2 ] || usage
        resolve "$2"
        # Removes the entry rather than setting it to `rw`, so access falls
        # back to the `video` group and there is one place that decides it.
        ${pkgs.acl}/bin/setfacl -x "u:$user" -- "$node"
        ;;

      *) usage ;;
    esac
  '';
in {
  # 1. WHO CAN OPEN THE CARD.
  #
  # KNOW WHAT THIS IS.  This machine's console is on that card.  Any process
  # running as the primary user can now become DRM master and blank the console
  # until it exits -- and on crux that user is also what the CI runner and the
  # interactive agent run as, so "any process" includes anything a workflow or
  # an agent executes.  There is no way to test modesetting without it; it is
  # written here so that it is a decision and not a surprise.
  #
  # crux is headless and reached over ssh, so a blanked console costs nothing
  # until the day something has gone wrong enough to need one.
  #
  # The quieter cost is that the 970 is not idle: nvr.nix sets
  # `ffmpeg.hwaccel_args = "preset-nvidia"`, so the house cameras decode on
  # this card.  A Chromium GPU process competes with those NVDEC sessions for
  # the card's memory, and that failure is dropped camera recording -- which
  # nobody notices the way they notice a dead console.  Worth knowing before
  # running a long session against the real card rather than vkms.
  primary-user.extraGroups = ["video"];

  # 2. SOMETHING TO LIGHT.
  #
  # vkms is the kernel's virtual KMS driver -- in-tree and expected to be built
  # as a module here, just not loaded.  It presents a *connected* connector
  # with real modes and an atomic CRTC, which is exactly the shape ozone/drm
  # wants and exactly what the 970's four dead connectors cannot offer.
  #
  # "expected to be built" is doing real work in that sentence and `modinfo
  # vkms` on the box settles it.  If it is not there, this line does not
  # degrade -- systemd-modules-load.service fails on every boot and the system
  # is permanently degraded over a module nothing else needs.
  #
  # It brings a second framebuffer with it, on a kernel new enough to give vkms
  # fbdev emulation -- but it cannot take the console, and that is worth
  # writing down so nobody "fixes" it.  fbcon claims a framebuffer only when it
  # is the *first* one registered; after that it moves only where
  # `con2fb_map_boot[]` sends it, which is to say only if `fbcon=map:` was
  # passed.  vkms comes up from systemd-modules-load, long after nvidia-drm has
  # taken fb0.  So do not add `fbcon=map:` here as a precaution: that parameter
  # is the one thing that could actually move the console onto a display nobody
  # can see, and unlike a DRM-master blank it does not end when a process
  # exits.
  #
  # This is the difference between the tty work being testable in CI forever
  # and being testable once, by hand, by someone who drove to the closet with a
  # monitor.
  boot = {
    kernelModules = ["vkms"];

    # None of the three is decoration:
    #
    #   enable_writeback  the writeback connector hands back the pixels that
    #                     were scanned out.  Without it a test can only say "it
    #                     did not crash"; with it there is a pixel verdict, the
    #                     same shape as the other guards in the domicile repo.
    #   enable_cursor     ozone/drm drives a hardware cursor plane (DrmCursor),
    #                     and that path has nothing to bind to without one.
    #   enable_overlay    ozone/drm's HardwareDisplayPlaneManagerAtomic and its
    #                     overlay manager enumerate planes.
    #
    # All three are named rather than left to their defaults, which have moved
    # between kernel releases before.  A test that quietly loses its pixel
    # verdict because writeback went away looks exactly like a passing test.
    extraModprobeConfig = ''
      options vkms enable_cursor=1 enable_writeback=1 enable_overlay=1
    '';
  };

  # 3. SWITCHING BETWEEN THE TWO CARDS WITHOUT A PERSON.
  #
  # Chromium picks its *primary* DRM device in
  # ui/ozone/platform/drm/host/drm_display_host_manager.cc: it walks
  # /dev/dri/card%d upward, stopping at the first index that does not exist but
  # only skipping past one it cannot open, keeps what reports count_crtcs > 0,
  # and takes the first, because GetPreferredDrmDrivers() is empty on anything
  # that is not a 2011 iMac.  There is no flag and no environment variable for
  # it.  The tree is on this machine (chromium-build.nix owns it), so this is a
  # grep rather than a memory -- check it there before trusting this paragraph
  # over the source.
  #
  # That "skips what it cannot open" is the whole mechanism: with both cards
  # openable the primary is always the blind nvidia one.
  #
  # Claim no more than that.  Ozone does take further DRM devices from udev and
  # does enumerate displays across all of them -- that is how multi-GPU works
  # -- so vkms's connected connector may well show up as a secondary without
  # any of this.  What the primary decides is what Chromium requires at startup
  # and drives the main GBM path through, and open-ability is the only lever on
  # which device that is.  That narrower thing is what makes this a mechanism
  # and not a convenience: without it every switch between the virtual card and
  # the real one is a message to a human and a wait.
  #
  #
  # IT LASTS UNTIL THE NEXT REBOOT, AND NOTHING PUTS IT BACK.
  #
  # /dev is devtmpfs: the kernel recreates these nodes on every boot, with no
  # ACL on them.  So a `hide` is good for one boot, and nothing here reapplies
  # it -- deliberately, because the handoff this implements asked for a switch
  # rather than a policy, and a boot-time default belongs in the file that
  # decides the policy rather than in the one that provides the lever.
  #
  # The failure that matters is silent: crux reboots (powerpanel.nix exists for
  # a reason), the hide is gone, and the next run tests against the blind card
  # and reports whatever a blind card reports.  `drm-acl status` is the check,
  # and a run that cares should make it rather than assume.
  #
  # logind can undo it inside a boot, too.  systemd tags `SUBSYSTEM=="drm",
  # KERNEL=="card*"` for uaccess, so a login on the physical console rewrites
  # the same named-user entry to `rw-` for that session -- and drops the entry
  # on logout rather than restoring the deny.  Only local seats, so ssh is not
  # affected; the case it does cover is somebody at the console during exactly
  # the emergency the note above contemplates.
  environment.systemPackages = [drm-acl];

  # The stable path rather than the store path the other entry in this
  # repository uses (config/modules/ui/log).  That one is invoked by a
  # generated command line carrying the same store path, so the two always
  # agree; this one is typed, by a person or an agent who will get whatever
  # PATH resolves -- which is this.
  #
  # The trade, stated the right way round: naming the symlink keeps the rule
  # valid across rebuilds, at the price that the rule names whatever that
  # symlink currently points at.  It retargets on any `nixos-rebuild
  # switch`/`test` or rollback, not only a colmena deploy -- so what is
  # authorised is "the drm-acl of the running system", which is the thing
  # meant.  A caller types the same path the rule names, so the two match
  # whether or not sudoers canonicalises before comparing.
  #
  # No argument list: the command takes any, because the script's own
  # validation is the guard and a sudoers pattern would be a second, weaker
  # copy of it.
  primary-user.sudo-cmds = ["/run/current-system/sw/bin/drm-acl"];

  # THIS DOES NOT REACH INSIDE A CI JOB, AND THE HANDOFF THAT ASKED FOR IT
  # ASSUMED IT WOULD.
  #
  # The reasoning was that the runner service runs as the primary user, so
  # anything granted to that user covers CI too.  That holds for file
  # permissions and for the `video` group above.  It does not hold for sudo:
  # the github-runner module sets NoNewPrivileges (mkDefault true) and
  # CapabilityBoundingSet (mkBefore, empty), and either alone is enough.  The
  # first makes the kernel ignore the setuid bit, which sudo detects and
  # reports by name.  The second is quieter: a setuid-root exec takes its
  # permitted set from the bounding set, so with that empty sudo reaches uid 0
  # holding no capabilities at all and cannot even setgroups().
  #
  # Left as is rather than punched through, because undoing both on a unit that
  # already runs trusted-workflows-only as a wheel user is a bigger decision
  # than this file should make on its own.  The interactive agent
  # (claude-agent.nix) and `ssh crux <cmd>` both have a normal PAM session and
  # are unaffected, so the switch has a home today; a job that needs the
  # virtual card needs the ACL already in the state it wants, set from there.
  #
  # If CI is to drive the switch itself, the choice is between relaxing those
  # two on the runner and going around them entirely -- a template unit plus a
  # polkit rule escalates out-of-process, which neither directive touches.
}
