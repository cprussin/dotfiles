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
  # imported by import-tank.service, so its datasets mount natively under
  # `zfs mount -a` rather than through fileSystems, and run-backup's
  # `zfs send -R` would replicate the tree to the external disk unless it is
  # added to that command's -X list.
  pool = "tank-fast";

  buildRoot = "/build";
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
  };

  # Chromium's toolchain is prebuilt dynamically-linked binaries fetched by
  # gclient hooks, which cannot run against the store unaided.  If one dies on
  # a missing .so, add it to programs.nix-ld.libraries -- systemPackages will
  # not help.
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
