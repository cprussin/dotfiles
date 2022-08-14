{pkgs, ...}: {
  services.dbus = {
    enable = true;
    packages = [pkgs.dconf];
  };
}
