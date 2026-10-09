{...}: {
  imports = [
    ./hardware.nix
    ../../profiles/physical-machine
    ../../profiles/server
    ../../modules/system/podman

    ./backup.nix
    ./chromium-build.nix
    ./circinus.nix
    ./claude-agent.nix
    ./dns.nix
    ./domicile-ci.nix
    ./domicile-tty.nix
    ./dynamic-dns.nix
    ./home-assistant.nix
    ./library.nix
    ./matrix.nix
    ./nvr.nix
    ./photos.nix
    ./powerpanel.nix
    ./private.nix
    ./syncthing.nix
    ./vaultwarden.nix
    ./wireguard.nix
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "crux";
    hostId = "a362c6ea";
  };
  environment.etc."machine-id".text = "bf6ba660172042baa958c54739b5fdb9\n";
  services = {
    getty.greetingLine = builtins.readFile ./greeting;
    fwupd.enable = true;
  };

  # Core dumps go to /var/lib/systemd/coredump, on the tmpfs root (see
  # hardware.nix), so they are RAM.  Crashing engines in CI left 2G there on
  # 2026-10-09.
  systemd.coredump.settings.Coredump.MaxUse = "1G";
}
