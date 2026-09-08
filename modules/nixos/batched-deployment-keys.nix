# Colmena uploads every key on every deploy, one at a time, whether or not the
# key changed, and the `<name>-key.service` units it generates watch their key
# file with inotify under `Restart=always`.  Replacing a key file therefore
# bounces its key unit, and through `Requires=` everything that depends on it.
# A deploy to a host with thirty keys bounces those consumers thirty times.
#
# That is merely wasteful until the deploy runs over one of the services being
# bounced: crux is reached over wireguard, wireguard depends on its key units,
# and colmena opens a fresh ssh connection per key -- so each upload knocks
# down the tunnel the next upload has to travel through.
#
# See https://github.com/nix-community/colmena/issues/174 and
# https://github.com/nix-community/colmena/issues/177.
#
# This module takes the restarts out of the upload path.  A key unit waits for
# its key, records the hash of what it came up with, and then stays up across
# replacements; a reconciler, once the deploy has gone quiet, restarts the key
# units whose file no longer hashes to what their unit started with.  Consumers
# keep depending on `<name>-key.service` exactly as before.
#
# The one behaviour lost is that deleting a key no longer stops its consumers.
# Keys under the default `/run/keys` still vanish on reboot, which stops
# everything anyway, and nothing here deletes keys otherwise.
{
  pkgs,
  lib,
  config,
  ...
}: let
  inherit (config.deployment) keys;

  # Where the keys actually land.  `destDir` is only the default for `path`,
  # which colmena lets a key set on its own.
  keyDir = key: builtins.dirOf key.path;

  keyDirs = lib.unique (lib.mapAttrsToList (_: keyDir) keys);

  # What each key unit came up with.  Deliberately under `/run`: the recorded
  # hash describes a running unit, so it should die with the units it describes
  # -- after a reboot every key unit starts again and records afresh.  Root-only
  # besides, since a hash of a secret confirms a guess at that secret.
  stateDir = "/run/colmena-key-hashes";

  # How often to look, and how long every key file has to have sat untouched,
  # before the upload counts as finished.  `colmena upload-keys` runs no
  # activation for us to watch for, so on that path this window is the only
  # thing standing between a restart and the uploads still to come -- it wants
  # to be comfortably longer than the gap between two uploads.
  poll = 10;
  quiet = 60;

  # Don't wait for quiet forever: something else writing to a key directory in
  # a loop would otherwise wedge the reconciler, and with it every later one,
  # since the path unit doesn't watch while the unit it triggered is running.
  maxWait = 900;

  keyList = ''
    declare -A colmenaKeys=(
    ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: key: "  [${lib.escapeShellArg name}]=${lib.escapeShellArg key.path}") keys)}
    )
  '';

  # Wait for the key rather than watching it, so that colmena replacing the
  # file underneath us doesn't take the unit -- and its consumers -- down.
  mkKeyService = name: key: let
    path = lib.escapeShellArg key.path;
    dir = lib.escapeShellArg (keyDir key);
  in
    lib.nameValuePair "${name}-key" {
      # Only what the script below calls: `mkForce` drops the default PATH
      # nixpkgs would otherwise add, so anything new needs adding here.
      path = lib.mkForce [pkgs.coreutils pkgs.inotify-tools];
      preStart = lib.mkForce "";
      script = lib.mkForce ''
        while [ ! -e ${path} ]; do
          # A directory colmena hasn't created yet is nothing to watch, and
          # inotifywait would just log an error about it every time round.
          if [ ! -d ${dir} ]; then
            sleep ${toString poll}
            continue
          fi
          status=0
          inotifywait -qq -t ${toString poll} -e create,moved_to ${dir} || status=$?
          # 0 is something appearing and 2 is the timeout; both just mean look
          # again.  Anything else means we couldn't watch after all, so pause
          # rather than spin.
          if [ "$status" -ne 0 ] && [ "$status" -ne 2 ]; then
            sleep ${toString poll}
          fi
        done

        # Record what this unit came up with, which is exactly what the
        # reconciler needs to tell an upload that changed the key from one that
        # rewrote the same bytes.  Recording it here rather than there is also
        # what makes a restart that fails get retried: the hash only moves when
        # the unit actually starts.
        mkdir -p ${stateDir}
        if hash="$(sha256sum <${path})"; then
          record=${stateDir}/${lib.escapeShellArg name}
          printf '%s' "''${hash%% *}" >"$record.tmp"
          mv "$record.tmp" "$record"
        fi
      '';

      # Left to itself, switch-to-configuration stops every changed key unit
      # before running the activation script and starts them again at the very
      # end -- so the deploy that lands a change to this module would take
      # wireguard down for the whole activation, over the very tunnel it is
      # running on.  Restarting instead puts the bounce after activation, in
      # one transaction, where it costs a second.
      stopIfChanged = false;

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        Restart = lib.mkForce "no";
        UMask = "0077";
      };
    };

  reconcileScript = ''
    set -u

    ${keyList}

    # The path unit stopped watching the moment it triggered us and won't fire
    # for writes that land while we run -- and we can be in here for a while,
    # waiting for the deploy and then for the consumers we restart to come
    # back.  So anything that could have hidden a write behind it asks for
    # another look itself, rather than leaving a rotated key sitting unnoticed
    # until the next deploy.  A look that finds nothing schedules nothing, so
    # the chain runs at most one look past the last change rather than forever.
    #
    # Asking twice is free -- the second finds the timer pending and says so --
    # but a real failure to schedule leaves nothing coming back to this, which
    # is worth a line in the journal either way.
    lookAgain() {
      systemd-run --quiet --collect --on-active=60 --unit=colmena-key-reconcile-retry \
        systemctl start --no-block colmena-key-reconcile.service ||
        echo "Could not schedule another look; one may already be pending."
    }

    # Colmena uploads the keys one at a time over a separate ssh connection
    # each, then runs switch-to-configuration behind them.  Restarting anything
    # before all of that finishes would drop the connection the deploy is
    # running over, which is the problem this module exists to avoid.
    deadline=$(($(date +%s) + ${toString maxWait}))
    busy=1
    while [ "$(date +%s)" -lt "$deadline" ]; do
      sleep ${toString poll}
      busy=""
      now="$(date +%s)"
      for name in "''${!colmenaKeys[@]}"; do
        mtime="$(stat -c %Y "''${colmenaKeys[$name]}" 2>/dev/null || echo 0)"
        # A key stamped in the future is one we can say nothing about, and
        # calling it busy would wedge us here for good.
        if [ "$mtime" -le "$now" ] && [ "$((now - mtime))" -lt ${toString quiet} ]; then
          busy=1
        fi
      done
      if pgrep -f switch-to-configuration >/dev/null; then
        busy=1
      fi
      if [ -z "$busy" ]; then
        break
      fi
    done

    # Out of patience with the deploy still going.  Restarting now is the one
    # thing we must not do, so leave it for the next look.
    if [ -n "$busy" ]; then
      echo "Gave up waiting for the deploy to settle; restarting nothing, will look again."
      lookAgain
      exit 0
    fi

    restart=()
    for name in "''${!colmenaKeys[@]}"; do
      path="''${colmenaKeys[$name]}"
      # A key that vanished under us is one to leave alone.
      if [ ! -e "$path" ] || ! hash="$(sha256sum <"$path")"; then
        continue
      fi
      hash="''${hash%% *}"
      if [ "$hash" = "$(cat ${stateDir}/"$name" 2>/dev/null || true)" ]; then
        continue
      fi
      restart+=("$name-key.service")
    done

    if [ "''${#restart[@]}" -eq 0 ]; then
      exit 0
    fi

    echo "Key contents changed, restarting: ''${restart[*]}"

    # Once before the restarts, so that one of them failing and taking this
    # script down with it still leaves a look pending -- that restart left its
    # key's hash stale, which is what makes the next look retry it.
    lookAgain

    # One systemctl call, so that systemd can coalesce a consumer of several
    # rotated keys into a single restart.  The units record their new hashes as
    # they come back up; a restart that fails leaves the old hash in place and
    # so gets retried next time round.
    systemctl restart "''${restart[@]}"

    # And once after, since restarts that outlast the retry's own delay would
    # otherwise have swallowed the look scheduled above.
    lookAgain
  '';
in {
  config = lib.mkIf (keys != {}) {
    systemd = {
      services =
        lib.mapAttrs' mkKeyService keys
        // {
          colmena-key-reconcile = {
            description = "Restart the key units whose keys actually changed";
            path = [pkgs.coreutils pkgs.procps config.systemd.package];
            script = reconcileScript;

            # Never let switch-to-configuration restart this: it would stop the
            # instance that is waiting for the deploy, start a fresh one, and
            # block on it -- while that new instance waits for the very
            # switch-to-configuration that is blocked on it.
            restartIfChanged = false;

            serviceConfig = {
              Type = "oneshot";
              # The script waits out the deploy it was triggered by, which
              # takes as long as the deploy takes.
              TimeoutStartSec = "infinity";
            };
          };
        };

      paths.colmena-key-reconcile = {
        wantedBy = ["paths.target"];
        pathConfig.PathChanged = keyDirs;
      };
    };
  };
}
