{...}: {
  imports = [
    ./hardware.nix
    ../../profiles/physical-machine
    ../../profiles/server

    ./backup.nix
    ./circinus.nix
    ./dns.nix
    ./dynamic-dns.nix
    ./home-assistant.nix
    ./library.nix
    ./libvirtd.nix
    ./matrix.nix
    ./nvr.nix
    ./photos.nix
    ./powerpanel.nix
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
  services.getty.greetingLine = builtins.readFile ./greeting;
}
