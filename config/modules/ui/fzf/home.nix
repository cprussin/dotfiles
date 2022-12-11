{
  lib,
  pkgs,
  ...
}: {
  home.packages = lib.mkForce [pkgs.fzf];

  programs.fzf = {
    enable = true;
    defaultOptions = ["--info=inline"];
  };
}
