{...}: {
  boot.kernelModules = ["sg"];
  primary-user.extraGroups = ["cdrom"];
}
