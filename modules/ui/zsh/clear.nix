{ writeShellScript, fortune, cowsay, lolcat, ncurses, ls }:

writeShellScript "clear" ''
  fortune=${fortune}/bin/fortune
  cowthink=${cowsay}/bin/cowthink
  lolcat=${lolcat}/bin/lolcat
  clear=${ncurses}/bin/clear

  $clear $@
  $fortune | $cowthink -n | $lolcat
  echo
  ${ls}
''
