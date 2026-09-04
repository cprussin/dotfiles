# See docs/casting-design.org for why any of this is shaped the way it is.
{
  writeShellScript,
  coreutils,
  gnugrep,
  systemd,
  sway,
  jq,
}:
writeShellScript "cast" ''
  cat=${coreutils}/bin/cat
  comm=${coreutils}/bin/comm
  head=${coreutils}/bin/head
  rm=${coreutils}/bin/rm
  seq=${coreutils}/bin/seq
  sleep=${coreutils}/bin/sleep
  sort=${coreutils}/bin/sort
  test=${coreutils}/bin/test
  grep=${gnugrep}/bin/grep
  systemctl=${systemd}/bin/systemctl
  swaymsg=${sway}/bin/swaymsg
  jq=${jq}/bin/jq

  outputFile="$XDG_RUNTIME_DIR/cast-output"

  # The TV.  `create_output` hardcodes 1920x1080, and Sunshine captures the
  # output's mode rather than its logical size, so this is what gets encoded --
  # leave it at the default and a 4K client just upscales 1080p.  Scale 2 keeps
  # the desktop readable from a couch and leaves the logical size at 1920x1080,
  # which is what the positioning below works in.
  mode=3840x2160
  scale=2

  # Headless only: a monitor plugged in between the two samples below would
  # otherwise land in the diff, and everything downstream takes a single name.
  headlessOutputs() {
    $swaymsg -t get_outputs | $jq -r '.[].name' | $grep '^HEADLESS-' | $sort
  }

  # Also runs before starting, because a cast can end without `cast` and the
  # output outlives it.
  unplug() {
    if $test -f "$outputFile"
    then
      $swaymsg "output $($cat "$outputFile") unplug" > /dev/null 2>&1
      $rm -f "$outputFile"
    fi
  }

  # Match the state, not `is-active --quiet`'s status: that calls `activating`
  # a failure, so a second `cast` during startup would start again.
  case "$($systemctl --user is-active sunshine)" in
    inactive | failed)
      unplug

      # A start outside `cast` exits 1 and can trip the unit's start limit,
      # which would leave the layout built around a service that never comes up.
      $systemctl --user reset-failed sunshine 2> /dev/null

      # `create_output` takes no arguments and names them `HEADLESS-N` with N
      # counting up, so the name has to be read back.
      before="$(headlessOutputs)"
      $swaymsg create_output > /dev/null
      output="$($comm -13 <(echo "$before") <(headlessOutputs) | $head -n 1)"

      if $test -z "$output"
      then
        echo 'cast: sway added no output; check the sway log' >&2
        exit 1
      fi

      # Record before anything else can fail.
      echo "$output" > "$outputFile"

      # swaymsg reports command errors on stdout, and as JSON rather than prose
      # when it is not a tty.  Keep them: the failure below otherwise points at
      # a sway log that has nothing in it.
      if ! error="$($swaymsg "output $output mode --custom $mode scale $scale" 2>&1)"
      then
        echo "cast: $(echo "$error" | $jq -r '.[].error? // .' 2> /dev/null || echo "$error")" >&2
        exit 1
      fi

      # The modeset runs behind a timer, and one that cannot be allocated drops
      # back to the old mode while keeping the new scale -- a quarter-size
      # desktop the client then upscales, which is the bug this is fixing
      # wearing a disguise.  Wait for the mode, which also settles the rect the
      # positioning below reads.
      for _ in $($seq 50)
      do
        outputs="$($swaymsg -t get_outputs)"
        current="$(echo "$outputs" | $jq -r --arg o "$output" \
          'first(.[] | select(.name == $o) | "\(.current_mode.width)x\(.current_mode.height)")')"
        $test "$current" = "$mode" && break
        $sleep 0.02
      done

      if $test "$current" != "$mode"
      then
        echo "cast: $output came up ''${current:-nothing}, not $mode" >&2
        exit 1
      fi

      read -r width height < <(echo "$outputs" | $jq -r --arg o "$output" \
        'first(.[] | select(.name == $o) | .rect | "\(.width) \(.height)")')

      # Belt and braces: the mode check above should already have caught every
      # way this can come back empty, `0` or `null`.  Any of them would abort
      # jq at `--argjson w` or collapse the column below to match nothing,
      # landing the TV on a real output.
      if ! $test "$width" -gt 0 2> /dev/null
      then
        echo "cast: $output reports no size; is it still enabled?" >&2
        exit 1
      fi

      # At the top of the focused output's column, so the pointer reaches the TV
      # off the top edge.  Only that column counts -- see the design notes.
      anchorX="$(echo "$outputs" | $jq -r --arg o "$output" \
        'map(select(.active and .focused and .name != $o)) | .[0].rect.x // 0')"
      topY="$(echo "$outputs" | $jq -r --arg o "$output" --argjson x "$anchorX" --argjson w "$width" \
        'map(select(.active and .name != $o and .rect.x < ($x + $w) and (.rect.x + .rect.width) > $x) | .rect.y) | min // 0')"
      $swaymsg "output $output position $anchorX $((topY - height))" > /dev/null

      # Covers workspace 10's next creation only, so an existing one still has
      # to be moved below.  Appends rather than replaces; sway skips names that
      # no longer resolve.
      $swaymsg "workspace 10 output $output" > /dev/null

      # Focus it to move it -- an empty workspace cannot be named any other way.
      # Gated, not chained: the switch fails under fullscreen-global, a chained
      # list ignores that, and the move would take whatever is focused instead.
      #
      # Restoring by output, not by workspace: if the user was already on
      # workspace 10, that name now resolves to the TV.
      focusedOutput="$(echo "$outputs" | $jq -r --arg o "$output" \
        'map(select(.active and .focused and .name != $o))[0].name // empty')"

      if $swaymsg "workspace --no-auto-back-and-forth 10" > /dev/null
      then
        $swaymsg "move workspace to output $output" > /dev/null

        # The move takes focus and the pointer with it.
        if $test -n "$focusedOutput"
        then
          $swaymsg "focus output \"$focusedOutput\"" > /dev/null
        fi
      else
        echo 'cast: could not move workspace 10 to the TV (fullscreen global?)' >&2
      fi

      $systemctl --user start sunshine
      ;;
    *)
      $systemctl --user stop sunshine
      unplug
      ;;
  esac
''
