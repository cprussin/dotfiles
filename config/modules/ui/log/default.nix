{pkgs, ...}: {
  primary-user.sudo-cmds = ["${pkgs.systemd}/bin/journalctl -alf"];
}
