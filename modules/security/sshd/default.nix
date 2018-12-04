{ lib, ... }:

{
  services.openssh = {
    enable = true;
    permitRootLogin = "no";
  };

  systemd.services.sshd.wantedBy = lib.mkForce [];
}
