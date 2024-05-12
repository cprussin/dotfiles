{...}: {
  imports = [
    ./hardware.nix
    ../../profiles/laptop
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "aries";
    hostId = "b616ba83";
  };
  environment.etc."machine-id".text = "70ef59137eb5416e835a6246288bbd32\n";
  services.getty.greetingLine = builtins.readFile ./greeting;
  services.fwupd.enable = true;
}
