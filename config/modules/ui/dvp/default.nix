{ config, ... }:

{
  keymap = {
    layout = "us";
    variant = "dvp";
    options = "caps:escape";
  };
  primary-user.home-manager.keymap = config.keymap;
}
