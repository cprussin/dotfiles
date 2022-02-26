{pkgs, ...}: {
  services.dbus = {
    enable = true;
    packages = [pkgs.gnome3.dconf];
  };
}
