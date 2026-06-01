{pkgs, ...}: {
  primary-user.extraGroups = ["adbusers"];
  environment.systemPackages = [pkgs.android-tools];
}
