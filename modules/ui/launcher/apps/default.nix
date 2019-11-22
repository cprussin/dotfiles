{ callPackage
, writeShellScript
, bluez
, gimp
, htop
, pavucontrol
, openssh
, slack
, qemu
, emacs
, coreutils
, steam
, systemd
, bitwig-studio
, config
}:

let
  mkWebApp = name: url: writeShellScript name "exec @out@/bin/browse ${url}";

  mkGoogleApp = name: url: writeShellScript name ''
    test=${coreutils}/bin/test
    browse=@out@/bin/browse

    user=0
    if $test "$1" == "netflix"
    then
      user=1
    fi

    url=${url}

    exec @out@/bin/browse ''${url/@user@/$user}
  '';

  mkTerminalApp = name: bin: writeShellScript name ''
    exec ${config.terminal} --title ${name} --name ${name} -e "${bin}"
  '';
in

{
  amazon = mkWebApp "amazon" "https://www.amazon.com/";
  aws = mkWebApp "aws" "https://console.aws.amazon.com/console/home?region=us-east-1#";
  bitwig = writeShellScript "bitwig" "${bitwig-studio}/bin/bitwig-studio";
  bluetooth = mkTerminalApp "bluetooth" "${bluez}/bin/bluetoothctl";
  brave = writeShellScript "firefox" "@out@/bin/browse --browser brave $*";
  calendar = mkGoogleApp "calendar" "https://www.google.com/calendar/b/@user@/render#main_7";
  chrome = writeShellScript "chrome" "@out@/bin/browse --browser chrome $*";
  chromium = writeShellScript "chromium" "@out@/bin/browse --browser chromium $*";
  emacs = writeShellScript "emacs" "@out@/bin/open \${1-*scratch*}";
  email = writeShellScript "email" "sh -c \"${systemd}/bin/systemctl --user start mbsync & ${emacs}/bin/emacsclient -c --eval '(mu4e~headers-jump-to-maildir \\\"/PrussinNet/Inbox\\\")'\"";
  firefox = writeShellScript "firefox" "@out@/bin/browse --browser firefox $*";
  gdrive = mkGoogleApp "gdrive" "https://drive.google.com/drive/u/@user@/my-drive";
  gimp = writeShellScript "gimp" "${gimp}/bin/gimp";
  github = mkWebApp "github" "https://github.com/";
  gmail = mkGoogleApp "gmail" "https://mail.google.com/mail/u/@user@";
  htop = mkTerminalApp "htop" "${htop}/bin/htop";
  journal = mkTerminalApp "journal" "sudo ${systemd}/bin/journalctl -alf";
  mixer = writeShellScript "mixer" "${pavucontrol}/bin/pavucontrol";
  netflix = mkWebApp "netflix" "http://www.netflix.com";
  netflix-api = callPackage ./netflix-api.nix {};
  opera = writeShellScript "firefox" "@out@/bin/browse --browser opera $*";
  "prussin.net" = mkTerminalApp "prussin.net" "${openssh}/bin/ssh prussin.net";
  reboot = writeShellScript "reboot" "@out@/bin/yes-no -m 'Are you sure you want to reboot?' -y 'Yes, reboot' -n 'No, remain on' -- ${systemd}/bin/systemctl reboot";
  remacs = callPackage ./remacs.nix {};
  reno = mkWebApp "shakti-reno" "https://map.builds.test.netflix.net/view/Reno/";
  screenshot = callPackage ./screenshot.nix {};
  shakti = callPackage ./shakti.nix { inherit config; };
  shutdown = writeShellScript "shutdown" "@out@/bin/yes-no -m 'Are you sure you want to shut down?' -y 'Yes, shut down' -n 'No, remain on' -- ${systemd}/bin/systemctl poweroff";
  slack = writeShellScript "slack" "${slack}/bin/slack";
  slackagain = writeShellScript "slackagain" "sh -c 'pkill -x slack; exec @out@/share/apps/slack'";
  sotd = callPackage ./sotd.nix {};
  steam = writeShellScript "steam" "${steam}/bin/steam";
  syncmail = callPackage ./syncmail.nix {};
  vpn = callPackage ./vpn.nix {};
  windows-7 = writeShellScript "windows-7" "${qemu}/bin/qemu-system-x86_64 -enable-kvm -machine type=pc,accel=kvm -cpu host -m 1G -usb -device usb-tablet \"$HOME/Software/Microsoft/Windows 7 VM.iso\"";
}
