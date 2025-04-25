{pkgs, ...}: {
  imports = [
    ./hardware.nix
    ../../profiles/laptop
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "lyra";
    hostId = "73898c5c";
  };
  environment.etc."machine-id".text = "89e4f9d000c74a389a33b82baa7c2fb2\n";
  services.getty.greetingLine = builtins.readFile ./greeting;
  services.fwupd.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_6_12;
}
