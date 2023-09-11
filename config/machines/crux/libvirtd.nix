{pkgs, ...}: {
  networking.firewall.interfaces.prussinnet.allowedTCPPorts = [5900];

  virtualisation.libvirtd = {
    enable = true;
    onBoot = "ignore";

    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = false;
      ovmf = {
        enable = true;
        packages = [pkgs.OVMFFull.fd];
      };
      swtpm.enable = true;
    };
  };

  primary-user.extraGroups = ["libvirtd"];
}
