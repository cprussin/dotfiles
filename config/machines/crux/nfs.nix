{pkgs, ...}: let
  network = pkgs.callPackage ../../../lib/network.nix {};
in {
  services.nfs.server = {
    enable = true;
    hostName = "${network.wireguard6.crux.address},${network.wireguard4.crux.address}";
    exports = ''
      /srv/Library *(ro,fsid=0,no_subtree_check,crossmnt,insecure,all_squash)
      "/srv/Library/Family Photos" *(rw,no_subtree_check,insecure,all_squash)
      "/srv/Library/Documents" *(rw,no_subtree_check,insecure,all_squash)
      "/srv/Library/Google Drive" *(rw,no_subtree_check,insecure,all_squash)
    '';
  };
  networking.firewall.interfaces.prussinnet.allowedTCPPorts = [2049];
}
