{ config, ... }:

let
  layout = "us";
  variant = "dvp";
in

{
  i18n.consoleKeyMap = variant;

  services.xserver = {
    inherit layout;
    xkbVariant = variant;
    xkbOptions = "caps:escape";
  };

  home-manager.users.${config.primaryUserName} = { ... }: {
    home.keyboard = {
      inherit layout variant;
    };
  };
}
