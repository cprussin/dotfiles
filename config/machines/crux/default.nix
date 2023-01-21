{...}: {
  imports = [
    ./hardware.nix
    ../../profiles/physical-machine
    ../../profiles/server

    ./backup.nix
    ./dns.nix
    ./dvd-ripping.nix
    ./dynamic-dns.nix
    ./home-assistant.nix
    ./library.nix
    ./matrix.nix
    ./nfs.nix
    ./syncthing.nix
    ./wireguard.nix
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "crux";
    hostId = "a362c6ea";
  };
  environment.etc."machine-id".text = "bf6ba660172042baa958c54739b5fdb9\n";
  services.getty.greetingLine = builtins.readFile ./greeting;
  programs.powerpanel = {
    enable = true;
    enable-alarm = false;
  };
}
