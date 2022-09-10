{...}: {
  imports = [
    ./hardware.nix
    ../../profiles/physical-machine
    ../../profiles/server

    ./backup.nix
    ./syncthing.nix
    ./dynamic-dns.nix
    ./acme.nix
    ./dvd-ripping.nix

    ./wireguard.nix
    ./dns.nix
    ./library.nix
    ./home-assistant.nix
    ./matrix.nix
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "crux";
    hostId = "a362c6ea";
  };
  environment.etc."machine-id".text = "bf6ba660172042baa958c54739b5fdb9\n";
  services.getty.greetingLine = builtins.readFile ./greeting;
  programs.powerpanel.enable = true;
}
