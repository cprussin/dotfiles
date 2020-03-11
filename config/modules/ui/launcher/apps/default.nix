{ callPackage
, writeShellScript
, bluez
, gimp
, htop
, pavucontrol
, openssh
, slack
, qemu
, steam
, sway
, systemd
, bitwig-studio
, emacs
, launcher
, fzf-pass
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
  brave = writeShellScript "firefox" "${launcher}/bin/browse --browser brave $*";
  calendar = mkGoogleApp "calendar" "https://www.google.com/calendar/b/@user@/render#main_7";
  chrome = writeShellScript "chrome" "${launcher}/bin/browse --browser chrome $*";
  chromium = writeShellScript "chromium" "${launcher}/bin/browse --browser chromium $*";
  dvp = writeShellScript "dvp" "${sway}/bin/swaymsg \"input * xkb_variant 'dvp'\"";
  emacs = writeShellScript "emacs" "${launcher}/bin/open \${1-*scratch*}";
  email = callPackage ./email.nix {};
  firefox = writeShellScript "firefox" "${launcher}/bin/browse --browser firefox $*";
  gdrive = mkGoogleApp "gdrive" "https://drive.google.com/drive/u/@user@/my-drive";
  gimp = "${gimp}/bin/gimp";
  github = mkWebApp "github" "https://github.com/";
  gmail = mkGoogleApp "gmail" "https://mail.google.com/mail/u/@user@";
  htop = mkTerminalApp "htop" "${htop}/bin/htop";
  journal = mkTerminalApp "journal" "sudo ${systemd}/bin/journalctl -alf";
  mixer = "${pavucontrol}/bin/pavucontrol";
  netflix = mkWebApp "netflix" "http://www.netflix.com";
  netflix-api = callPackage ./netflix-api.nix {};
  passwords = mkModal "passwords" "${fzf-pass}/bin/fzf-pass";
  "prussin.net" = mkTerminalApp "prussin.net" "${openssh}/bin/ssh prussin.net";
  reboot = mkConfirmationDialog "reboot" "Yes, reboot" "No, remain on" "Are you sure you want to reboot?" "${systemd}/bin/systemctl reboot";
  remacs = callPackage ./remacs.nix {};
  reno = mkWebApp "shakti-reno" "https://map.builds.test.netflix.net/view/Reno/";
  screenshot = callPackage ./screenshot.nix {};
  shakti = callPackage ./shakti.nix { inherit config; };
  shutdown = mkConfirmationDialog "shutdown" "Yes, shut down" "No, remain on" "Are you sure you want to shut down?" "${systemd}/bin/systemctl poweroff";
  slack = "${slack}/bin/slack";
  slackagain = writeShellScript "slackagain" "pkill -x slack; exec ${slack}/bin/slack";
  sotd = callPackage ./sotd.nix {};
  steam = "${steam}/bin/steam";
  syncmail = callPackage ./syncmail.nix {};
  syncthing = mkWebApp "syncthing" "http://localhost:8384";
  us = writeShellScript "dvp" "${sway}/bin/swaymsg \"input * xkb_variant ''\"";
  vpn = callPackage ./vpn.nix {};
  windows-7 = writeShellScript "windows-7" "${qemu}/bin/qemu-system-x86_64 -enable-kvm -machine type=pc,accel=kvm -cpu host -m 1G -usb -device usb-tablet \"$HOME/Software/Microsoft/Windows 7 VM.iso\"";
}
