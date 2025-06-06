{
  pkgs,
  lib,
  ...
}: let
  load-session = pkgs.writeShellScriptBin "load-session" ''
    tmux=${pkgs.tmux}/bin/tmux

    exec $tmux new-session -A -s 0
  '';
in {
  primary-user.home-manager = {
    home.packages = lib.mkForce [load-session];
    programs.tmux = {
      enable = true;
      clock24 = true;
      escapeTime = 0;
      keyMode = "vi";
      extraConfig = "set -g mouse on";
    };
  };
}
