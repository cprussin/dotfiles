# Twice a day, walk both Amex and both Chase accounts and add every offer the
# issuers are handing out. The bank logins live in pass and are assembled into
# the one JSON document the service reads; systemd stages it in a tmpfs only
# this unit can see, so nothing lands in the environment or in `systemctl show`.
{
  config,
  pkgs,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
in {
  deployment.keys.offer-adder-credentials = {
    keyCommand =
      passwords.getOfferAdderCredentials "Connor/Infrastructure/ntfy/offer-adder"
      {
        connor-amex = "Connor/Financial/American Express";
        connor-chase = "Connor/Financial/Chase";
        shauna-amex = "Shauna/Financial/American Express";
        shauna-chase = "Shauna/Financial/Chase";
      };
    # /run/keys is a tmpfs that colmena refills on deploy, so a reboot without
    # an `upload-keys` would silently break every run until the next deploy.
    destDir = "/secrets";
  };

  services.offer-adder = {
    enable = true;
    credentialFile = config.deployment.keys.offer-adder-credentials.path;
    # colmena hands over the plaintext document; systemd still stages it in the
    # per-unit tmpfs, it just has no TPM sealing to undo first.
    sealed = false;
    # A challenge that reaches nobody costs the account its run -- ntfy waits
    # five minutes for a reply -- so fire when somebody is awake rather than at
    # the small hours the module defaults to.
    onCalendar = "*-*-* 09:17,19:17:00";
    settings = {
      # Every account is `["ntfy"]` because neither issuer offers
      # authenticator-app enrollment: a challenge always ends up on a phone.
      # `"totp"` is not a preference that falls through to the rung behind it
      # -- the ladder resolves the secret up front and fails the account for
      # the whole run when there is none -- so it goes in only for an account
      # actually enrolled, alongside a `TOTP Secret` field in its pass entry.
      ntfy = {
        server = "http://${config.services.ntfy-sh.settings.listen-http}";
        alertTopic = "offers-alerts";
        replyTopic = "offers-codes";
      };
      accounts = [
        {
          id = "connor-amex";
          label = "Connor · Amex";
          issuer = "amex";
          senderHints = ["americanexpress" "american express"];
          codeSources = ["ntfy"];
        }
        {
          id = "connor-chase";
          label = "Connor · Chase";
          issuer = "chase";
          senderHints = ["chase.com" "chase"];
          codeSources = ["ntfy"];
        }
        {
          id = "shauna-amex";
          label = "Shauna · Amex";
          issuer = "amex";
          senderHints = ["americanexpress" "american express"];
          codeSources = ["ntfy"];
        }
        {
          id = "shauna-chase";
          label = "Shauna · Chase";
          issuer = "chase";
          senderHints = ["chase.com" "chase"];
          codeSources = ["ntfy"];
        }
      ];
    };
  };

  # The service's StateDirectory holds one Chromium profile per account, and
  # those profiles are the whole reason the banks stop challenging every login.
  # `/` here is a tmpfs, so they live on `tank/persisted-state/offer-adder`,
  # created with `mountpoint=/var/lib/offer-adder` and mounted natively by
  # `zfs mount -a` like the rest of crux's service state -- not declared in
  # `fileSystems`, which is for the tank-fast datasets.  `StateDirectory`
  # chowns the dataset root to the service user on each start, so nothing
  # here has to.  It is excluded from `run-backup` in ./backup.nix, and the
  # borg job is opt-in on `net.prussin:backup` -- a user property, so confirm
  # no ancestor sets it (`zfs get -s local,inherited net.prussin:backup tank
  # tank/persisted-state`) rather than assuming an unset child is enough.
  systemd.services.offer-adder = {
    # `import-tank` is what mounts that dataset; without it the timer's
    # catch-up run at boot writes profiles to the tmpfs root, and the later
    # `zfs mount -a` silently shadows what it wrote.  ntfy is only wanted:
    # requiring it would refuse the whole unit on a systemd dependency error,
    # where wanting it lets the run happen and report which accounts could not
    # get a code and why -- and a run only needs ntfy for an account the bank
    # actually challenges, which most runs are not.
    after = [
      "offer-adder-credentials-key.service"
      "import-tank.service"
      "ntfy-sh.service"
    ];
    requires = ["offer-adder-credentials-key.service" "import-tank.service"];
    wants = ["ntfy-sh.service"];
  };
}
