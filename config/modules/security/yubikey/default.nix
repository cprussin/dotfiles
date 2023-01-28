{
  pkgs,
  lib,
  ...
}: {
  primary-user.home-manager.systemd.user.services.yubikey-touch-detector = {
    Unit = {
      Description = "Yubikey touch detector & notifier";
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.yubikey-touch-detector}/bin/yubikey-touch-detector -libnotify";
      Restart = "on-failure";
      Environment = ["PATH=${lib.makeSearchPath "bin" [pkgs.gnupg]}"];
    };

    Install.WantedBy = ["default.target"];
  };
}
