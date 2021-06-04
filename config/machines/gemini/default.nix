{ ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/laptop
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "gemini";
    hostId = "72b0fa88";
  };
  environment.etc."machine-id".text = "1e07c7df74514d51872dac89ab314e7c\n";
  services.getty.greetingLine = builtins.readFile ./greeting;
}
