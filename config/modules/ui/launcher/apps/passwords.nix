{ callPackage
, findutils
, fzf
, less
, gnused
, pass
, coreutils
, gnugrep
, wl-clipboard
, stdenv
, launcher
, config
}:

let
  mkModal = callPackage ./utils/mkModal.nix { inherit config; };
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
  browse=${launcher}/bin/browse

  passwordFiles() {
    $find ${config.primary-user.secure.passwords} -type f -not -path "*/\.*"
  }

  passwords() {
    passwordFiles | $less | $sed 's|${config.primary-user.secure.passwords}/\(.*\).gpg|\1|'
  }

  getFields() {
    fields=$($echo -e "$1" | $sed '1d;/^otpauth/d' | $cut -d ':' -f 1)

    if $test "$($echo -e "$1" | head -n 1)"
    then
      $echo "Password"
    fi

    if $test "$($echo -e "$1" | $grep '^otpauth')"
    then
      $echo "OTP"
    fi

    if $test "$($echo -e "$1" | $grep '^URL: ')"
    then
        $echo "Go to site"
    fi

    $echo -e "$fields"
  }

  readField() {
    if $test "$3" = "Password"
    then
      $echo -e "$2" | $sed -n '1p'
    elif $test "$3" = "OTP"
    then
      $pass otp "$1"
    elif $test "$3" = "Go to site"
    then
      $nohup $browse "$($echo -e "$2" | $sed -n "/^URL: /p" | $sed "s/^URL: //")" &
      disown
      exit
    elif $test "$3"
    then
      $echo -e "$2" | $sed -n "/^$3/p" | $sed "s/^$3: //"
    fi
  }

  password=$(passwords | $fzf --layout reverse)
  if $test "$password"
  then
    data="$($pass show "$password")"
    field="$(getFields "$data" | $fzf --layout reverse)"
    fieldData="$(readField "$password" "$data" "$field")"
    if $test "$fieldData"
    then
      if $test -f $XDG_RUNTIME_DIR/password-clipboard.pid
      then
        kill -9 $(cat $XDG_RUNTIME_DIR/password-clipboard.pid)
        rm $XDG_RUNTIME_DIR/password-clipboard.pid
      fi
      $nohup $shell -c "$echo ''${fieldData@Q} | $wlCopy --trim-newline; sleep 15; $wlCopy --clear; rm $XDG_RUNTIME_DIR/password-clipboard.pid" >/dev/null &
      echo $! > $XDG_RUNTIME_DIR/password-clipboard.pid
      disown
    fi
  fi
''
