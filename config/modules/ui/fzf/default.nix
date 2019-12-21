{ pkgs, lib, ... }:

{
  primary-user.home-manager = {
    home.packages = lib.mkForce [ pkgs.fzf ];

    programs.fzf = {
      enable = true;
      inline-info = true;
    };
  };
}
