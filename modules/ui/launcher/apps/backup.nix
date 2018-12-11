{ writeScript, bash, coreutils, dnsutils, unison }:

writeScript "backup" ''
  #! ${bash}/bin/sh

  dig=${dnsutils}/bin/dig
  test=${coreutils}/bin/test
  unison=${unison}/bin/unison

  publicIp=$($dig +short myip.opendns.com @resolver1.opendns.com)
  homeIp=$($dig +short home.prussin.net @resolver1.opendns.com)

  if $test $publicIp == $homeIp
  then
      PROFILE=home
  else
      PROFILE=home-external
  fi

  exec $TERMINAL -e $unison $PROFILE
''
