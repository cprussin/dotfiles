{ writeShellScript, systemd, emacs }:

writeShellScript "email" ''
  systemctl=${systemd}/bin/systemctl
  emacsclient=${emacs}/bin/emacsclient

  $systemctl --user start mbsync &
  $emacsclient -c --eval '(mu4e~headers-jump-to-maildir "/PrussinNet/Inbox")'
''
