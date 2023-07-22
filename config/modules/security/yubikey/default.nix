{
  pkgs,
  lib,
  ...
}: {
  services.pcscd.enable = true;

  # These mobules break my NFC reader...
  boot.blacklistedKernelModules = ["pn533" "pn533_usb"];

  primary-user.home-manager = {
    systemd.user.services.yubikey-touch-detector = {
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

    programs.gpg.scdaemonSettings = {
      disable-ccid = true;
      pcsc-driver = "${lib.getLib pkgs.pcsclite}/lib/libpcsclite${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}";
    };
  };
}
