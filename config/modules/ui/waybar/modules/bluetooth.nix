{ pkgs, config }:

let
  bluetoothctl = "${pkgs.bluez}/bin/bluetoothctl";

  bluetooth = pkgs.writeShellScript "bluetooth" ''
    function devices() {
      ${bluetoothctl} devices 2>/dev/null |\
        grep Device |\
        sed 's/Device \([^ ]*\) .*/info \1/' |\
        ${bluetoothctl} 2>/dev/null |\
        grep -B 7 'Connected: yes' |\
        grep Alias |\
        sed 's/.*Alias: //' |\
        paste -s - |\
        sed 's/\t/, /'
    }

    if [ "$(${bluetoothctl} show 2>/dev/null | grep Powered | grep yes)" ]
    then
      devs=$(devices)
      if [ "$devs" ]
      then
        echo "{\"text\":\" $devs\"}"
      else
        echo "{\"text\":\" $devs\",\"class\":\"disconnected\"}"
      fi
    else
      echo "{\"text\":\"\",\"class\":\"off\"}"
    fi
  '';

  launcher-apps = pkgs.callPackage ../../launcher/apps { inherit config; };
in

{
  name = "custom/bluetooth";

  config = {
    exec = bluetooth;
    on-click = "${launcher-apps}/bluetooth";
    interval = 1;
    return-type = "json";
  };

  style.".disconnected".color = config.colorTheme.foreground;
  style.".off".color = config.colorTheme.warn;
}
