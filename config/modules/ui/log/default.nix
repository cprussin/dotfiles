{ pkgs, ... }:

{
  sudoCmds = [ "${pkgs.systemd}/bin/journalctl -alf" ];
}
