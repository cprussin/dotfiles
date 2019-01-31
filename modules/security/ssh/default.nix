{ pkgs, ... }:

{
  home.file.".ssh/known_hosts".source = ./known_hosts;
  home.file.".ssh/authorized_keys".source = ./authorized_keys;
  home.file.".ssh/environment".text = "PATH=${pkgs.git}/bin";

  programs.ssh = {
    enable = true;
    matchBlocks = [
      {
        host = "home.prussin.net 10.0.0.11";
        user = "cprussin";
        port = 5822;
      }
      {
        host = "prussin.net *.bci-incorporated.com";
        user = "connor";
        port = 3580;
      }
      {
        host = "*.prussin.net songoftheday.net";
        user = "root";
        port = 7534;
      }
      {
        host = "aur.archlinux.org";
        user = "aur";
      }
    ];
  };
}
