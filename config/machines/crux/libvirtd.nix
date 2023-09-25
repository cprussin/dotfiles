{pkgs, ...}: {
  networking.firewall.interfaces.prussinnet.allowedTCPPorts = [5900];

  boot.kernelParams = ["intel_iommu=on"];
  boot.kernelModules = ["vfio" "vfio_iommu_type1" "vfio_pci" "vfio_virqfd"];

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
