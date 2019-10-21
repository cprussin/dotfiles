{ config, ... }:

{
  services.xserver = {
    enable = true;

    videoDrivers = [ "intel" ];

    displayManager.lightdm = {
      enable = true;
      autoLogin.enable = true;
      autoLogin.user = config.primaryUserName;
    };
  };
}
