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
  screen = pkgs.callPackage ./apps/screen.nix {};

  email = mkWebApp "gmail" "https://mail.google.com?authuser=connor@prussin.net";
  sms = mkWebApp "sms" "https://messages.google.com/web/conversations";
  matrix = pkgs.writeShellScript "matrix" "${pkgs.element-desktop}/bin/element-desktop --ozone-platform-hint=auto";
  slack = pkgs.writeShellScript "slack" "${pkgs.slack}/bin/slack --ozone-platform-hint=auto -g warn";
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

    programs = {
      launcher = {
        enable = true;
        apps = {
          inherit email sms matrix slack discord zulip comms;

          agenda = pkgs.writeShellScript "agenda" "${pkgs.emacs}/bin/emacsclient -c -e '(org-agenda nil \"a\")'";
          bitwig = "${pkgs.bitwig-studio}/bin/bitwig-studio";
          bluetooth = mkTerminalApp "bluetooth" "${pkgs.bluez}/bin/bluetoothctl";
          brave = pkgs.writeShellScript "brave" "${pkgs.launcher}/bin/browse --browser brave $*";
          brightness = pkgs.callPackage ./apps/brightness.nix {};
          btop = mkTerminalApp "btop" "${pkgs.btop}/bin/btop";
          calendar = mkWebApp "calendar" "https://calendar.google.com?authuser=connor@prussin.net";
          chrome = pkgs.writeShellScript "chrome" "${pkgs.launcher}/bin/browse --browser chrome $*";
          chromium = pkgs.writeShellScript "chromium" "${pkgs.launcher}/bin/browse --browser chromium $*";
          connect-to-network = mkModal "connect-to-network" "${pkgs.connect-to-network}/bin/connect-to-network ${config.interfaces.wifi}";
          credit-cards = mkWebApp "credit-cards" "https://docs.google.com/spreadsheets/d/1Y8xind-5nMe9bezMFmk__CQdkSBd7FPupt1NkdKDLUE";
          crux = mkTerminalApp "crux" "${pkgs.openssh}/bin/ssh -t crux load-session";
          cups = mkWebApp "cups" "http://localhost:631";
          dvp = pkgs.writeShellScript "dvp" "${pkgs.sway}/bin/swaymsg \"input * xkb_variant 'dvp'\"";
          emacs = pkgs.writeShellScript "emacs" "${pkgs.launcher}/bin/open \${1-*scratch*}";
          firefox = pkgs.writeShellScript "firefox" "${pkgs.launcher}/bin/browse --browser firefox $*";
          gimp = "${pkgs.gimp}/bin/gimp";
          home = mkWebApp "home" "https://home-assistant.internal.prussin.net";
          journal = mkTerminalApp "journal" "sudo ${pkgs.systemd}/bin/journalctl -alf";
          library = mkWebApp "library" "https://library.internal.prussin.net";
          lock = pkgs.writeShellScript "lock" "${pkgs.systemd}/bin/loginctl lock-session";
          makemkv = "${pkgs.makemkv}/bin/makemkv";
          mic-volume = pkgs.callPackage ./apps/mic-volume.nix {};
          mixer = pkgs.writeShellScript "mixer" "exec ${pkgs.pavucontrol}/bin/pavucontrol";
          nix-shell = pkgs.writeShellScript "nix-shell" "exec nix-shell -p $1 --run \"$*\"";
          passwords = mkModal "passwords" "${pkgs.fzf-pass}/bin/fzf-pass";
          presentation = pkgs.callPackage ./apps/presentation.nix {};
          printotron = mkWebApp "printotron" "https://printotron.internal.prussin.net";
          reboot = mkConfirmationDialog "reboot" "Yes, reboot" "No, remain on" "Are you sure you want to reboot?" "${pkgs.systemd}/bin/systemctl reboot";
          remacs = pkgs.callPackage ./apps/remacs.nix {};
          scan = "${pkgs.xsane}/bin/xsane";
          screen-record = pkgs.writeShellScript "screenshot" "${screen} record $*";
          screenshot = pkgs.writeShellScript "screenshot" "${screen} shot $*";
          shutdown = mkConfirmationDialog "shutdown" "Yes, shut down" "No, remain on" "Are you sure you want to shut down?" "${pkgs.systemd}/bin/systemctl poweroff";
          sotd = mkWebApp "sotd" "https://docs.google.com/spreadsheets/d/168kHAuFM2bOHaQvyzkbWBF4206jV5bXpg0ubT3fSSJk";
          steam = "${pkgs.steam}/bin/steam";
          stop-screen-record = pkgs.writeShellScript "stop-screen-record" "pkill wf-recorder";
          syncthing = mkWebApp "syncthing" "http://localhost:8384";
          systemctl = mkModal "systemctl" "${pkgs.sysz}/bin/sysz";
          us = pkgs.writeShellScript "us" "${pkgs.sway}/bin/swaymsg \"input * xkb_variant ''\"";
          virt-manager = "${pkgs.virt-manager}/bin/virt-manager";
          volume = pkgs.callPackage ./apps/volume.nix {};
        };
      };

      emacs.emacs-rc.browse = "${pkgs.launcher}/bin/browse";
    };
  };
}
