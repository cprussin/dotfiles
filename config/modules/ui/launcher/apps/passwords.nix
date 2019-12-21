{ callPackage
, terminal
, secure
, findutils
, fzf
, less
, gnused
, pass
, coreutils
, gnugrep
, wl-clipboard
, stdenv
}:

let
  mkModal = callPackage ./utils/mkModal.nix { inherit terminal; };
in

mkModal "passwords" ''
  find=${findutils}/bin/find
  fzf=${fzf}/bin/fzf
  less=${less}/bin/less
  sed=${gnused}/bin/sed
  pass=${pass}/bin/pass
  cut=${coreutils}/bin/cut
  test=${coreutils}/bin/test
  echo=${coreutils}/bin/echo
  nohup=${coreutils}/bin/nohup
  grep=${gnugrep}/bin/grep
  wlCopy=${wl-clipboard}/bin/wl-copy
  shell=${stdenv.shell}

  passwordFiles() {
    $find ${secure.passwords} -type f -not -path "*/\.*"
  }

  passwords() {
    passwordFiles | $less | $sed 's|${secure.passwords}/\(.*\).gpg|\1|'
  }

  getFields() {
    fields=$($echo -e "$1" | $sed '1d;/^otpauth/d' | $cut -d ':' -f 1)

    if $test "$($echo -e "$1" | $grep '^otpauth')"
    then
      $echo -e "Password\nOTP\n$fields"
    else
      $echo -e "Password\n$fields"
    fi
  }

  readField() {
    if $test "$3" = "Password"
    then
      $echo -e "$2" | $sed -n '1p'
    elif $test "$3" = "OTP"
    then
      $pass otp "$1"
    elif $test "$3"
    then
      $echo -e "$2" | $sed -n "/^$3/p" | $sed "s/^$3: //"
    fi
  }

  password=$(passwords | $fzf --preview "" --layout reverse)
  if $test "$password"
  then
    data="$($pass show "$password")"
    field="$(getFields "$data" | $fzf --preview "" --layout reverse)"
    fieldData="$(readField "$password" "$data" "$field")"
    if $test "$fieldData"
    then
      pkill wl-copy
      $nohup $shell -c "$echo ''${fieldData@Q} | $wlCopy --trim-newline; sleep 15; $wlCopy --clear" >/dev/null &
      disown
    fi
  fi
''
