{ config, ... }:

{
  services.xserver = {
    enable = true;

    displayManager.lightdm = {
      enable = true;
      autoLogin.enable = true;
      autoLogin.user = config.primaryUserName;
    };
  };
}
