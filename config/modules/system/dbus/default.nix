{ pkgs, ... }:

{
  services.dbus = {
    enable = true;
    socketActivated = true;
    packages = [ pkgs.gnome3.dconf ];
  };
}
