{ writeShellScriptBin, bash, xorg, coreutils, gnugrep, gawk, systemd, gnused }:

writeShellScriptBin "setup-monitors" ''
  xrandr=${xorg.xrandr}/bin/xrandr
  test=${coreutils}/bin/test
  echo=${coreutils}/bin/echo
  grep=${gnugrep}/bin/grep
  awk=${gawk}/bin/awk
  systemctl=${systemd}/bin/systemctl
  cut=${coreutils}/bin/cut
  sed=${gnused}/bin/sed

  monitors=$($xrandr -q)

  function isconnected() {
      $test ! "$($echo "$monitors" | $grep $1 | $grep 'disconnected')"
  }

  function disconnect() {
      $echo "$1" | $awk -e '{print "--output " $0 " --off "}'
  }

  function setupMonitors() {

      # Run xrandr with the arguments passed in to this function.
      $xrandr $@

      # Reset apps that get messed up when the monitor configuration changes.
      $systemctl --user start random-background
      $systemctl --user restart tray

      # Now exit the process--we only want to set up the first connection since we
      # run through displays in a specific priority ordering.
      exit

  }

  DISPLAYS=$($xrandr -q | $cut -d ' ' -f 1 | $sed '1d;/^$/d')
  HDMI_DISPLAYS=$($echo "$DISPLAYS" | $grep "HDMI")
  PANEL_DISPLAY=$($echo "$DISPLAYS" | $grep "eDP")
  DP_DISPLAYS=$($echo "$DISPLAYS" | $grep "DP" | $sed "/$PANEL_DISPLAY/d")

  for display in $HDMI_DISPLAYS
  do
      if isconnected $display
      then
          setupMonitors \
              --output $PANEL_DISPLAY --auto --primary \
              --output $display --auto --above $PANEL_DISPLAY \
              $(disconnect "$DP_DISPLAYS") \
              $(disconnect "$($echo "$HDMI_DISPLAYS" | $sed "/$display/d")")
      fi
  done

  for display in $DP_DISPLAYS
  do
      if isconnected $display
      then
          setupMonitors \
              --output $display --auto --primary \
              --output $PANEL_DISPLAY --auto --left-of $display \
              $(disconnect "$HDMI_DISPLAYS") \
              $(disconnect "$($echo "$DP_DISPLAYS" | $sed "/$display/d")")
      fi
  done

  setupMonitors \
      --output $PANEL_DISPLAY --auto --primary \
      $(disconnect "$HDMI_DISPLAYS") \
      $(disconnect "$DP_DISPLAYS")
''
