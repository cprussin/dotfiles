{ pkgs, lib, config, ... }:
let
  mkWebApp = pkgs.callPackage ./utils/mkWebApp.nix { };
  mkGoogleApp = pkgs.callPackage ./utils/mkGoogleApp.nix { };
  mkTerminalApp' = pkgs.callPackage ./utils/mkTerminalApp.nix { inherit config; };
  mkTerminalApp = name: mkTerminalApp' name name;
  mkModal = pkgs.callPackage ./utils/mkModal.nix { inherit config; };
  mkConfirmationDialog = pkgs.callPackage ./utils/mkConfirmationDialog.nix { inherit config; };

  email = mkGoogleApp "gmail" "https://mail.google.com?authuser=@user@";
in
{
  primary-user.home-manager = {
    home.packages = lib.mkForce [ pkgs.launcher ];

    programs.launcher = {
      enable = true;
      apps = {
        inherit email;

        agenda = pkgs.writeShellScript "agenda" "${pkgs.emacs}/bin/emacsclient -c -e '(org-agenda-list)'";
        amazon = mkWebApp "amazon" "https://www.amazon.com/";
        aws = mkWebApp "aws" "https://console.aws.amazon.com/console/home?region=us-east-1#";
        bitwig = "${pkgs.bitwig-studio}/bin/bitwig-studio";
        bluetooth = mkTerminalApp "bluetooth" "${pkgs.bluez}/bin/bluetoothctl";
        brave = pkgs.writeShellScript "brave" "${pkgs.launcher}/bin/browse --browser brave $*";
        brightness = pkgs.callPackage ./apps/brightness.nix { };
        calendar = mkGoogleApp "calendar" "https://calendar.google.com?authuser=@user@";
        chrome = pkgs.writeShellScript "chrome" "${pkgs.launcher}/bin/browse --browser chrome $*";
        chromium = pkgs.writeShellScript "chromium" "${pkgs.launcher}/bin/browse --browser chromium $*";
        crux = mkTerminalApp "crux" "${pkgs.openssh}/bin/ssh -t crux load-session";
        discord = "${pkgs.discord}/bin/discord";
        dvp = pkgs.writeShellScript "dvp" "${pkgs.sway}/bin/swaymsg \"input * xkb_variant 'dvp'\"";
        emacs = pkgs.writeShellScript "emacs" "${pkgs.launcher}/bin/open \${1-*scratch*}";
        firefox = pkgs.writeShellScript "firefox" "${pkgs.launcher}/bin/browse --browser firefox $*";
        gdrive = mkGoogleApp "gdrive" "https://drive.google.com?authuser=@user@";
        gimp = "${pkgs.gimp}/bin/gimp";
        github = mkWebApp "github" "https://github.com/";
        gmail = email;
        home = mkWebApp "home" "https://720-natoma-drive.prussin.net";
        htop = mkTerminalApp "htop" "${pkgs.htop}/bin/htop";
        insecure = pkgs.writeShellScript "secure" "sudo ${config.primary-user.secure.exportCmd}";
        journal = mkTerminalApp "journal" "sudo ${pkgs.systemd}/bin/journalctl -alf";
        library = mkWebApp "library" "https://library.prussin.net";
        lock = pkgs.callPackage ./apps/lock.nix { inherit config; };
        makemkv = "${pkgs.makemkv}/bin/makemkv";
        mic-volume = pkgs.callPackage ./apps/mic-volume.nix { };
        mixer = pkgs.writeShellScript "mixer" "exec ${pkgs.pavucontrol}/bin/pavucontrol";
        nix-shell = pkgs.writeShellScript "nix-shell" "exec nix-shell -p $1 --run \"$*\"";
        passwords = mkModal "passwords" "${pkgs.fzf-pass}/bin/fzf-pass";
        plex = mkWebApp "plex" "https://app.plex.tv/desktop";
        presentation = pkgs.callPackage ./apps/presentation.nix { };
        reboot = mkConfirmationDialog "reboot" "Yes, reboot" "No, remain on" "Are you sure you want to reboot?" "${pkgs.systemd}/bin/systemctl reboot";
        remacs = pkgs.callPackage ./apps/remacs.nix { };
        scan = "${pkgs.xsane}/bin/xsane";
        screenshot = pkgs.callPackage ./apps/screenshot.nix { };
        secure = pkgs.writeShellScript "secure" "sudo ${config.primary-user.secure.importCmd}";
        shutdown = mkConfirmationDialog "shutdown" "Yes, shut down" "No, remain on" "Are you sure you want to shut down?" "${pkgs.systemd}/bin/systemctl poweroff";
        signal = "${pkgs.signal-desktop}/bin/signal-desktop";
        slack = pkgs.writeShellScript "slack" "${pkgs.slack}/bin/slack -g warn";
        slackagain = pkgs.writeShellScript "slackagain" "pkill -x slack; exec ${config.primary-user.home-manager.programs.launcher.apps.slack}";
        sms = mkWebApp "sms" "https://messages.google.com/web/conversations";
        sotd = mkWebApp "sotd" "https://docs.google.com/spreadsheets/d/11yYp4Ma5t7wJxSBZQYyVcO7FlWuG6cEOJrPXcQCv3AI";
        steam = "${pkgs.steam}/bin/steam";
        syncthing = mkWebApp "syncthing" "http://localhost:8384";
        telegram = "${pkgs.tdesktop}/bin/telegram-desktop";
        us = pkgs.writeShellScript "us" "${pkgs.sway}/bin/swaymsg \"input * xkb_variant ''\"";
        volume = pkgs.callPackage ./apps/volume.nix { };
        zulip = "${pkgs.zulip}/bin/zulip";
      };
    };
  };
}
