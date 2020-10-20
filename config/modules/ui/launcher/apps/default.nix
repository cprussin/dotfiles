{ callPackage
, writeShellScript
, bluez
, gimp
, htop
, pavucontrol
, openssh
, slack
, steam
, sway
, systemd
, bitwig-studio
, emacs
, launcher
, fzf-pass
, utillinux
, config
}:

let
  mkWebApp = callPackage ./utils/mkWebApp.nix {};
  mkGoogleApp = callPackage ./utils/mkGoogleApp.nix {};
  mkTerminalApp' = callPackage ./utils/mkTerminalApp.nix { inherit config; };
  mkTerminalApp = name: mkTerminalApp' name name;
  mkModal = callPackage ./utils/mkModal.nix { inherit config; };
  mkConfirmationDialog = callPackage ./utils/mkConfirmationDialog.nix { inherit config; };
in

{
  agenda = writeShellScript "agenda" "${emacs}/bin/emacsclient -c -e '(org-agenda-list)'";
  amazon = mkWebApp "amazon" "https://www.amazon.com/";
  aws = mkWebApp "aws" "https://console.aws.amazon.com/console/home?region=us-east-1#";
  bitwig = "${bitwig-studio}/bin/bitwig-studio";
  bluetooth = mkTerminalApp "bluetooth" "${bluez}/bin/bluetoothctl";
  brave = writeShellScript "brave" "${launcher}/bin/browse --browser brave $*";
  calendar = mkGoogleApp "calendar" "https://www.google.com/calendar/b/@user@/render#main_7";
  chrome = writeShellScript "chrome" "${launcher}/bin/browse --browser chrome $*";
  chromium = writeShellScript "chromium" "${launcher}/bin/browse --browser chromium $*";
  crux = mkTerminalApp "crux" "${openssh}/bin/ssh -t crux load-session";
  dvp = writeShellScript "dvp" "${sway}/bin/swaymsg \"input * xkb_variant 'dvp'\"";
  emacs = writeShellScript "emacs" "${launcher}/bin/open \${1-*scratch*}";
  email = callPackage ./email.nix {};
  firefox = writeShellScript "firefox" "${launcher}/bin/browse --browser firefox $*";
  gdrive = mkGoogleApp "gdrive" "https://drive.google.com/drive/u/@user@/my-drive";
  gimp = "${gimp}/bin/gimp";
  github = mkWebApp "github" "https://github.com/";
  gmail = mkGoogleApp "gmail" "https://mail.google.com/mail/u/@user@";
  home = mkWebApp "home" "https://720-natoma-drive.prussin.net";
  htop = mkTerminalApp "htop" "${htop}/bin/htop";
  insecure = writeShellScript "secure" "sudo ${utillinux}/bin/umount /secure";
  journal = mkTerminalApp "journal" "sudo ${systemd}/bin/journalctl -alf";
  mixer = writeShellScript "mixer" "exec ${pavucontrol}/bin/pavucontrol";
  netflix = mkWebApp "netflix" "http://www.netflix.com";
  netflix-api = callPackage ./netflix-api.nix {};
  nix-shell = writeShellScript "nix-shell" "exec nix-shell -p $1 --run \"$*\"";
  passwords = mkModal "passwords" "${fzf-pass}/bin/fzf-pass";
  plex = mkWebApp "plex" "https://app.plex.tv/desktop";
  "prussin.net" = mkTerminalApp "prussin.net" "${openssh}/bin/ssh prussin.net";
  reboot = mkConfirmationDialog "reboot" "Yes, reboot" "No, remain on" "Are you sure you want to reboot?" "${systemd}/bin/systemctl reboot";
  remacs = callPackage ./remacs.nix {};
  screenshot = callPackage ./screenshot.nix {};
  secure = writeShellScript "secure" "sudo ${utillinux}/bin/mount /secure";
  shakti = callPackage ./shakti.nix { inherit config; };
  shutdown = mkConfirmationDialog "shutdown" "Yes, shut down" "No, remain on" "Are you sure you want to shut down?" "${systemd}/bin/systemctl poweroff";
  slack = "${slack}/bin/slack";
  slackagain = writeShellScript "slackagain" "pkill -x slack; exec ${slack}/bin/slack";
  sotd = mkWebApp "sotd" "https://docs.google.com/spreadsheets/d/11yYp4Ma5t7wJxSBZQYyVcO7FlWuG6cEOJrPXcQCv3AI";
  steam = "${steam}/bin/steam";
  syncmail = callPackage ./syncmail.nix {};
  syncthing = mkWebApp "syncthing" "http://localhost:8384";
  us = writeShellScript "us" "${sway}/bin/swaymsg \"input * xkb_variant ''\"";
  vpn = callPackage ./vpn.nix {};
}
