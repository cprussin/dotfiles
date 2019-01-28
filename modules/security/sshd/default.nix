{ lib, ... }:

{
  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    extraConfig = "PermitUserEnvironment yes";
  };

  systemd.services.sshd.wantedBy = lib.mkForce [];
}
