{
  writeShellScript,
  brightnessctl,
  notify-send,
}:
writeShellScript "brightness" ''
  brightnessctl="${brightnessctl}/bin/brightnessctl"
  notifySend="${notify-send}/bin/notify-send"

  showBrightness() {
    level=$((100 * $($brightnessctl get) / $($brightnessctl max)))

    if [ $level -eq 100 ]
    then
      icon=notification-display-brightness-full
    elif [ $level -ge 50 ]
    then
      icon=notification-display-brightness-high
    elif [ $level -ge 25 ]
    then
      icon=notification-display-brightness-medium
    elif [ $level -gt 0 ]
    then
      icon=notification-display-brightness-low
    else
      icon=notification-display-brightness-off
    fi

    $notifySend \
      -R "$XDG_RUNTIME_DIR/brightness.msgid" \
      -i $icon \
      -h int:value:$level \
      ""
  }

  raiseBrightness() {
    level=$((100 * $($brightnessctl get) / $($brightnessctl max)))
    if [ $level -le 10 ]
    then
      $brightnessctl set 1%+
    else
      $brightnessctl set 5%+
    fi
    showBrightness
  }

  lowerBrightness() {
    level=$((100 * $($brightnessctl get) / $($brightnessctl max)))
    if [ $level -le 10 ]
    then
      $brightnessctl set 1%-
    else
      $brightnessctl set 5%-
    fi
    showBrightness
  }

  case "$1" in
    up) raiseBrightness ;;
    down) lowerBrightness ;;
    *)
      echo "ERROR: Invalid option $1.  Valid options: up, down." >&2;
      exit 1
      ;;
  esac
''
