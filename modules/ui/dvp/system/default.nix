{ ... }:

{
  i18n.consoleKeyMap = "dvp";
  services.xserver = {
    layout = "us";
    xkbVariant = "dvp";
    xkbOptions = "caps:escape";
  };
}
