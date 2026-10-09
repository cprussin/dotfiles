{
  config,
  pkgs,
  ...
}: {
  services.flatpak.enable = true;

  # services.flatpak asserts the NixOS-level portal is on, and that in turn
  # asserts a backend.  Routing stays where ui/xdg-portal and ui/domicile put
  # it, in home-manager.  Nothing here may write `portals.conf`: the frontend
  # stops at the first directory holding either `<desktop>-portals.conf` or
  # `portals.conf`, and /etc/xdg comes before the profiles that carry
  # `domicile-portals.conf`, so a `common` entry would route a domicile desk
  # to gtk.  `sway` is named only to silence the module's warning about an
  # empty config; it repeats ui/sway's choice, which home-manager writes to
  # ~/.config where it is read first anyway.
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.sway.default = ["wlr" "gtk"];
  };

  # NixOS adds no remote, so `flatpak install` has nowhere to install from.
  # Flathub, system-wide, once the network is up; `--if-not-exists` makes
  # every boot after the first a no-op.  Retried, since a boot without the
  # network would otherwise leave no remote until the next one.
  systemd.services.flatpak-flathub = {
    description = "Add the Flathub remote to Flatpak";
    wantedBy = ["multi-user.target"];
    wants = ["network-online.target"];
    after = ["network-online.target"];
    path = [config.services.flatpak.package];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = "1min";
    };
    script = ''
      flatpak remote-add --system --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
    '';
  };
}
