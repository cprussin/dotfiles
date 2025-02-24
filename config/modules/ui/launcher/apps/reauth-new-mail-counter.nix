{
  writeShellScript,
  launcher,
  gmail-new-mail-counter,
}:
writeShellScript "reauth-new-mail-counter" ''
  reauth() {
    while read line
    do
      if [[ $line == https://* ]]
      then
        ${launcher}/bin/browse "$line"
      fi
    done < <(${gmail-new-mail-counter}/bin/gmail_new_mail_counter --auth "$1" --auth-format "{{{url}}}")
  }

  reauth connor@dourolabs.xyz
  reauth connor@prussin.net
''
