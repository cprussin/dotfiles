{ callPackage
, writeScript
, bash
, bluez
, toggle-colors
, setxkbmap
, gimp
, htop
, pavucontrol
, keepassxc
, openssh
, message
, slack
, qemu
, emacs
, secrets
, coreutils
}:

let
  mkWebApp = name: url: writeScript name ''
    #! ${bash}/bin/sh
    exec @out@/bin/browse ${url}
  '';

  mkGoogleApp = name: url: writeScript name ''
    #! ${bash}/bin/sh

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

  mkTerminalApp = name: bin: writeScript name ''
    #! ${bash}/bin/sh
    exec $TERMINAL -name ${name} -e ${bin}
  '';

  mkScript = name: script: writeScript name ''
    #! ${bash}/bin/sh
    exec ${script}
  '';
in

# TODO use absolute paths here for systemctl/journalctl/sudo/etc...
{
  amazon = mkWebApp "amazon" "https://www.amazon.com/";
  authenticate = callPackage ./authenticate.nix { inherit secrets; };
  backup = callPackage ./backup.nix {};
  bluetooth = mkTerminalApp "bluetooth" "${bluez}/bin/bluetoothctl";
  calendar = mkGoogleApp "calendar" "https://www.google.com/calendar/b/@user@/render#main_7";
  dark = mkScript "dark" "${toggle-colors}/bin/toggle-colors dark";
  dvorak = mkScript "dvorak" "${setxkbmap}/bin/setxkbmap us dvp";
  emacs = mkScript "emacs" "@out@/bin/open \${1-*scratch*}";
  email = mkScript "email" "sh -c \"systemctl --user start mbsync & ${emacs}/bin/emacsclient -c --eval '(mu4e~headers-jump-to-maildir \\\"/PrussinNet/Inbox\\\")'\"";
  gaming = callPackage ./gaming.nix {};
  gdrive = mkGoogleApp "gdrive" "https://drive.google.com/drive/u/@user@/my-drive";
  gimp = mkScript "gimp" "${gimp}/bin/gimp";
  github = mkWebApp "github" "https://github.com/";
  gmail = mkGoogleApp "gmail" "https://mail.google.com/mail/u/@user@";
  htop = mkTerminalApp "htop" "${htop}/bin/htop";
  jira = mkWebApp "jira" "https://jira.netflix.com/secure/Dashboard.jspa?selectPageId=22387#";
  journal = mkTerminalApp "journal" "sudo journalctl -alf";
  light = mkScript "light" "${toggle-colors}/bin/toggle-colors light";
  localhost = mkWebApp "localhost" "http://localhost:\${1-4200}";
  mixer = mkScript "mixer" "${pavucontrol}/bin/pavucontrol";
  netflix = callPackage ./netflix.nix {};
  passwords = mkScript "passwords" "${keepassxc}/bin/keepassxc";
  "prussin.net" = mkTerminalApp "prussin.net" "${openssh}/bin/ssh prussin.net";
  reboot = mkScript "reboot" "@out@/bin/yes-no -m 'Are you sure you want to reboot?' -y 'Yes, reboot' -n 'No, remain on' 'systemctl reboot'";
  remacs = mkScript "remacs" "${message}/bin/message write 'Restarting emacs...' 'systemctl --user restart emacs-daemon'";
  screenshot = callPackage ./screenshot.nix {};
  shakti = callPackage ./shakti.nix {};
  shutdown = mkScript "shutdown" "@out@/bin/yes-no -m 'Are you sure you want to shut down?' -y 'Yes, shut down' -n 'No, remain on' 'systemctl shutdown'";
  slack = mkScript "slack" "${slack}/bin/slack";
  slackagain = mkScript "slackagain" "sh -c 'pkill -x slack; ${slack}/bin/slack'";
  sotd = callPackage ./sotd.nix {};
  stash = mkWebApp "stash" "https://stash.corp.netflix.com";
  syncmail = mkScript "syncmail" "${message}/bin/message write 'Syncing email...' 'systemctl --user start mbsync'";
  trello = mkWebApp "trello" "https://trello.com/b/mHhKhvTF/web-core";
  us = mkScript "us" "${setxkbmap}/bin/setxkbmap us";
  vpn = callPackage ./vpn.nix {};
  windows-7 = mkScript "windows-7" "${qemu}/bin/qemu-system-x86_64 -enable-kvm -machine type=pc,accel=kvm -cpu host -m 1G -net nic -net user,smb=$HOME/Software -usb -device usb-tablet $HOME/Software/Microsoft/Windows\ 7\ VM.iso";
}
