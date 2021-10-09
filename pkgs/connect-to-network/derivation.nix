{ writeShellScriptBin, wpa_supplicant, fzf }:

writeShellScriptBin "connect-to-network" ''
  wpa_cli=${wpa_supplicant}/bin/wpa_cli
  fzf=${fzf}/bin/fzf

  wpa="$wpa_cli -p /var/run/wpa_supplicant -i $1"

  $wpa scan >/dev/null
  # sleep 2 # TODO how to know when wpa_cli scan is actually complete?
  ssid=$($wpa scan_results | cut -f 5- | sed '1d;/^$/d' | sort | uniq | fzf --layout=reverse --prompt "Select a network: ")
  if [ ! "$ssid" ]
  then
    echo "ERROR: No network selected!"
    exit 1
  fi
  password=$(echo "" | fzf --layout=reverse --print-query --prompt "Enter password: ")

  network=$($wpa add_network)
  $wpa set_network $network ssid "\"$ssid\"" >/dev/null
  $wpa set_network $network psk "\"$password\"" >/dev/null
  $wpa enable_network $network >/dev/null
  $wpa select_network $network >/dev/null
''
