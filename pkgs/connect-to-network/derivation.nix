{ writeShellScriptBin, wpa_supplicant, fzf }:

let
  wpa_cli = "${wpa_supplicant}/bin/wpa_cli";
  fzf' = "${fzf}/bin/fzf";
in

writeShellScriptBin "connect-to-network" ''
  device="$1"
  if [ ! "$device" ]
  then
    device="$(ip link | grep wlp | head -n 1 | cut -d ' ' -f 2 | sed 's/:$//')"
  fi

  wpa="${wpa_cli} -p /var/run/wpa_supplicant -i $device"

  $wpa scan >/dev/null
  # sleep 2 # TODO how to know when wpa_cli scan is actually complete?
  results="$($wpa scan_results)"
  ssid="$(echo "$results" | cut -f 5- | sed '1d;/^$/d' | sort | uniq | ${fzf'} --layout=reverse --prompt "Select a network: ")"
  if [ ! "$ssid" ]
  then
    echo "ERROR: No network selected!"
    exit 1
  fi

  network=$($wpa add_network)
  $wpa set_network $network ssid "\"$ssid\"" >/dev/null

  if [ "$(echo "$results" | grep "$ssid\$" | head -n 1 | cut -f 4 | grep PSK)" ]
  then
    password=$(echo "" | ${fzf'} --layout=reverse --print-query --prompt "Enter password: ")
    $wpa set_network $network psk "\"$password\"" >/dev/null
  else
    $wpa set_network $network key_mgmt NONE >/dev/null
  fi

  echo Enabling network...

  $wpa enable_network $network >/dev/null
  $wpa select_network $network >/dev/null
''
