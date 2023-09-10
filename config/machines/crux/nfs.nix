{pkgs, ...}: let
  network = pkgs.callPackage ../../../lib/network.nix {};
in {
  services.nfs.server = {
    enable = true;
    exports = ''
      /srv/Library ${network.wireguard.id.cidr}(ro,fsid=0,no_subtree_check,crossmnt,insecure,all_squash)
      "/srv/Library/Family Photos" ${network.wireguard.id.cidr}(rw,no_subtree_check,insecure,all_squash)
      "/srv/Library/Documents" ${network.wireguard.id.cidr}(rw,no_subtree_check,insecure,all_squash)
      "/srv/Library/Google Drive" ${network.wireguard.id.cidr}(rw,no_subtree_check,insecure,all_squash)
    '';
  };
  networking.firewall.interfaces.prussinnet.allowedTCPPorts = [2049];
}
