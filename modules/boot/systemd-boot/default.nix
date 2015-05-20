{ ... }:

{
  boot.loader = {
    systemd-boot = {
      enable = true;
      editor = false;
    };
    efi.canTouchEfiVariables = true;
  };
}
