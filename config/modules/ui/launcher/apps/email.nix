{ writeShellScript, systemd, emacs }:

writeShellScript "email" ''
  systemctl=${systemd}/bin/systemctl
  emacsclient=${emacs}/bin/emacsclient

  $emacsclient -c --eval '(mu4e~headers-jump-to-maildir "/PrussinNet/Inbox")'
''
