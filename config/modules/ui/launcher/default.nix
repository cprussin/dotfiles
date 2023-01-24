{
  pkgs,
  lib,
  config,
  ...
}: let
  mkWebApp = pkgs.callPackage ./utils/mkWebApp.nix {};
  mkTerminalApp' = pkgs.callPackage ./utils/mkTerminalApp.nix {inherit config;};
  mkTerminalApp = name: mkTerminalApp' name name;
  mkModal = pkgs.callPackage ./utils/mkModal.nix {inherit config;};
  mkConfirmationDialog = pkgs.callPackage ./utils/mkConfirmationDialog.nix {inherit config;};

  email = mkWebApp "gmail" "https://mail.google.com?authuser=connor@prussin.net";
  sms = mkWebApp "sms" "https://messages.google.com/web/conversations";
  matrix = pkgs.writeShellScript "matrix" "${pkgs.element-desktop}/bin/element-desktop --enable-features=UseOzonePlatform --ozone-platform=wayland";
  slack = pkgs.writeShellScript "slack" "${pkgs.slack}/bin/slack --enable-features=UseOzonePlatform --ozone-platform=wayland -g warn";
  discord = "${pkgs.discord}/bin/discord";
  zulip = "${pkgs.zulip}/bin/zulip";

  comms = pkgs.writeShellScript "" ''
    ${email} &
    ${sms} &
    ${matrix} &
    ${slack} &
  '';
in {
  nixpkgs.overlays = [
    (import ../../../../pkgs/launcher/overlay.nix)
  ];

  primary-user.home-manager = {
    home.packages = lib.mkForce [pkgs.launcher];

    programs.launcher = {
      enable = true;
      apps = {
        inherit email sms matrix slack discord zulip comms;

        agenda = pkgs.writeShellScript "agenda" "${pkgs.emacs}/bin/emacsclient -c -e '(org-agenda-list)'";
        amazon = mkWebApp "amazon" "https://www.amazon.com/";
        aws = mkWebApp "aws" "https://console.aws.amazon.com/console/home?region=us-east-1#";
        bitwig = "${pkgs.bitwig-studio}/bin/bitwig-studio";
        bluetooth = mkTerminalApp "bluetooth" "${pkgs.bluez}/bin/bluetoothctl";
        brave = pkgs.writeShellScript "brave" "${pkgs.launcher}/bin/browse --browser brave $*";
        brightness = pkgs.callPackage ./apps/brightness.nix {};
        calendar = mkWebApp "calendar" "https://calendar.google.com?authuser=connor@prussin.net";
        chrome = pkgs.writeShellScript "chrome" "${pkgs.launcher}/bin/browse --browser chrome $*";
        chromium = pkgs.writeShellScript "chromium" "${pkgs.launcher}/bin/browse --browser chromium $*";
        connect-to-network = mkModal "connect-to-network" "${pkgs.connect-to-network}/bin/connect-to-network ${config.interfaces.wifi}";
        crux = mkTerminalApp "crux" "${pkgs.openssh}/bin/ssh -t crux load-session";
        dvp = pkgs.writeShellScript "dvp" "${pkgs.sway}/bin/swaymsg \"input * xkb_variant 'dvp'\"";
        emacs = pkgs.writeShellScript "emacs" "${pkgs.launcher}/bin/open \${1-*scratch*}";
        firefox = pkgs.writeShellScript "firefox" "${pkgs.launcher}/bin/browse --browser firefox $*";
        gdrive = mkWebApp "gdrive" "https://drive.google.com?authuser=connor@prussin.net";
        gimp = "${pkgs.gimp}/bin/gimp";
        github = mkWebApp "github" "https://github.com/";
        gmail = email;
        home = mkWebApp "home" "https://home-assistant.internal.prussin.net";
        htop = mkTerminalApp "htop" "${pkgs.htop}/bin/htop";
        insecure = pkgs.writeShellScript "insecure" "sudo ${config.primary-user.secure.exportCmd}";
        journal = mkTerminalApp "journal" "sudo ${pkgs.systemd}/bin/journalctl -alf";
        library = mkWebApp "library" "https://library.internal.prussin.net";
        lock = pkgs.writeShellScript "lock" "${pkgs.systemd}/bin/loginctl lock-session";
        makemkv = "${pkgs.makemkv}/bin/makemkv";
        mic-volume = pkgs.callPackage ./apps/mic-volume.nix {};
        mixer = pkgs.writeShellScript "mixer" "exec ${pkgs.pavucontrol}/bin/pavucontrol";
        nix-shell = pkgs.writeShellScript "nix-shell" "exec nix-shell -p $1 --run \"$*\"";
        passwords = mkModal "passwords" "${pkgs.fzf-pass}/bin/fzf-pass";
        plex = mkWebApp "plex" "https://app.plex.tv/desktop";
        presentation = pkgs.callPackage ./apps/presentation.nix {};
        printotron = mkWebApp "printotron" "https://printotron.internal.prussin.net";
        reboot = mkConfirmationDialog "reboot" "Yes, reboot" "No, remain on" "Are you sure you want to reboot?" "${pkgs.systemd}/bin/systemctl reboot";
        remacs = pkgs.callPackage ./apps/remacs.nix {};
        scan = "${pkgs.simple-scan}/bin/simple-scan";
        screenshot = pkgs.callPackage ./apps/screenshot.nix {};
        secure = pkgs.writeShellScript "secure" "sudo ${config.primary-user.secure.importCmd}";
        shutdown = mkConfirmationDialog "shutdown" "Yes, shut down" "No, remain on" "Are you sure you want to shut down?" "${pkgs.systemd}/bin/systemctl poweroff";
        sotd = mkWebApp "sotd" "https://docs.google.com/spreadsheets/d/11yYp4Ma5t7wJxSBZQYyVcO7FlWuG6cEOJrPXcQCv3AI";
        steam = "${pkgs.steam}/bin/steam";
        syncthing = mkWebApp "syncthing" "http://localhost:8384";
        us = pkgs.writeShellScript "us" "${pkgs.sway}/bin/swaymsg \"input * xkb_variant ''\"";
        volume = pkgs.callPackage ./apps/volume.nix {};
      };
    };
  };
}
