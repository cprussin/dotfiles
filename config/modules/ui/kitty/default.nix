{ pkgs, config, ... }:

{
  primary-user.home-manager = {
    default-terminal = {
      enable = true;
      bin = "${pkgs.kitty}/bin/kitty";
      pkg = pkgs.kitty;
      termname = "xterm-kitty";
    };

    programs.kitty = {
      enable = config.primary-user.home-manager.default-terminal.enableApplication;
      settings.open_url_with = "${pkgs.launcher}/bin/browse";
    };
  };
}
