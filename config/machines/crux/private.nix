{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
in {
  deployment.keys = {
    "private.internal.prussin.net.crt" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/private.internal.prussin.net/cert";
      user = config.users.users.nginx.name;
    };
    "private.internal.prussin.net.key" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/private.internal.prussin.net/key";
      user = config.users.users.nginx.name;
    };
  };

  services.nginx.virtualHosts."private.internal.prussin.net" = {
    listenAddresses = ["[${network.wireguard6.crux.address}]" "${network.wireguard4.crux.address}"];
    sslCertificate = config.deployment.keys."private.internal.prussin.net.crt".path;
    sslCertificateKey = config.deployment.keys."private.internal.prussin.net.key".path;
    forceSSL = true;
    http2 = true;
    locations."/" = {
      proxyPass = "http://localhost:42042";
      proxyWebsockets = true;
      recommendedProxySettings = true;
    };
  };

  systemd.services.nginx = {
    requires = [
      "private.internal.prussin.net.crt-key.service"
      "private.internal.prussin.net.key-key.service"
    ];
    after = [
      "private.internal.prussin.net.crt-key.service"
      "private.internal.prussin.net.key-key.service"
    ];
  };

  networking.firewall.interfaces.prussinnet.allowedTCPPorts = [42042];

  # A system unit rather than a home-manager user unit.  The user unit couldn't
  # express what it needed: `Requires=`/`After=` sat in `[Install]`, a section
  # where systemd knows no such keys and drops them, and a user unit can't order
  # itself against a system unit like `import-tank` even from `[Unit]`.  Nor
  # would it stay up -- without lingering the user manager only exists between
  # login and logout, so `default.target` never came up at boot and every unit
  # under it died with the last session.
  systemd.services.private = {
    description = "Private";
    wantedBy = ["multi-user.target"];

    # `/` is a tmpfs and `/home` comes from tank, so the checkout doesn't exist
    # until `import-tank` has run `zfs mount -a`.  `requires` propagates that
    # unit's stop to this one; `after` is what orders this unit's stop ahead of
    # the `zpool export` in its `preStop`, which nothing released the pool for
    # before.
    #
    # `network-online.target` isn't for the server, which needs no address to
    # exist before it binds -- nginx doesn't wait on it either.  It's for `nix
    # develop`, which re-evaluates the flake on every start and, after the
    # weekly `nix.gc`, may have to refetch to do it.
    requires = ["import-tank.service"];
    after = [
      "import-tank.service"
      "network-online.target"
    ];
    wants = ["network-online.target"];

    # Nix still shells out to `git` for some flake refs, and it does that while
    # evaluating -- with this PATH, before the devShell exists.  (`nix develop`
    # then prepends the devShell's PATH rather than replacing it, so this stays
    # reachable as the tail.)
    path = [pkgs.git];

    serviceConfig = {
      Type = "simple";
      User = config.primary-user.name;

      # `WorkingDirectory` rather than the old `/bin/sh -c 'cd ~/Private && ...'`.
      # Not for supervision: NixOS's `/bin/sh` is bash, which execs the last
      # command of a `-c` list, so `nix` was already the main process either way.
      # It's that the directory is declared where `systemctl show` reports it,
      # instead of buried in a string only a shell can read.
      WorkingDirectory = "${config.primary-user.home}/Private";
      ExecStart = "${pkgs.nix}/bin/nix develop --command cli start-prod";

      # A server that stops serving is a failure however it exited, so
      # `on-failure` was wrong: a clean exit left the port dead until someone
      # noticed.  `RestartSec` is the other half of it -- at the 100ms default,
      # five restarts fit inside the limiter's 10s window and systemd then
      # refuses to start the unit at all until `systemctl reset-failed`.  Spaced
      # 10s apart the limiter can't be reached, so there's nothing to disable.
      # Nothing surfaces as a `failed` unit either -- a dependency failure fails
      # the start job, not the unit -- so an outage shows up in the journal.
      Restart = "always";
      RestartSec = "10s";
    };
  };
}
