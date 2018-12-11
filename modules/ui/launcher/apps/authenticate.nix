{ writeScript, bash, secrets, coreutils, xdotool, oathToolkit }:

writeScript "authenticate" ''
  #! ${bash}/bin/sh

  ls=${coreutils}/bin/ls
  cat=${coreutils}/bin/cat
  prompt=@out@/bin/prompt
  test=${coreutils}/bin/test
  xdotool=${xdotool}/bin/xdotool
  oathtool=${oathToolkit}/bin/oathtool

  PROMPT="Authenticate for which app?"

  app=$($ls "${secrets}/totp" | $prompt -lines 1 -p "$PROMPT ")
  if $test "$app"
  then
    $xdotool type "$($oathtool --totp -b $($cat "${secrets}/totp/$app"))"
    $xdotool key Return
  fi
''
