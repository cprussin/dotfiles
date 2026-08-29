{
  writeShellScript,
  claude-desktop,
  coreutils,
  jq,
  notify-send,
  sway,
  systemd,
}:
# Claude Desktop closes to the tray rather than quitting, and launching it again
# starts a second copy instead of coming back to the one that's already there.
# So don't launch it when it's already running: raise what's running instead.
#
# Two ways to raise it, because a closed window is gone from sway's tree
# entirely -- it isn't hidden or minimized, the surface is destroyed -- and
# there is nothing left to focus.  With a window still open we focus it; with
# only the tray icon left we click that icon's own "open" entry over D-Bus,
# which is the same path a mouse click through waybar takes.
writeShellScript "claude" ''
  busctl=${systemd}/bin/busctl
  claudeDesktop=${claude-desktop}/bin/claude-desktop
  date=${coreutils}/bin/date
  jq=${jq}/bin/jq
  notifySend=${notify-send}/bin/notify-send
  sleep=${coreutils}/bin/sleep
  swaymsg=${sway}/bin/swaymsg
  test=${coreutils}/bin/test

  # Every container lists its children in `focus` most-recently-used first, so
  # descending that rather than `nodes` walks the windows in MRU order and the
  # first match is the Claude window last looked at.  Nothing in the scratchpad
  # is reachable that way -- sway builds `focus` from the seat's focus stack,
  # which drops containers with no workspace -- so the plain scan follows as a
  # fallback.  Repeats past that point cost nothing: the loop stops at the
  # first Claude window either list turns up.
  windows='
    def leaves:
      (.focus // []) as $order
      | ((.nodes // []) + (.floating_nodes // [])) as $children
      | if ($children | length) == 0
        then .
        else ($order[] as $id | $children[] | select(.id == $id) | leaves)
        end;
    (leaves | select(.pid)), (.. | objects | select(.pid and .id))
    | "\(.pid):\(.id)"
  '

  # Pick the menu entry that reopens the window.  The label is the app's to
  # choose and we don't get to see it from here, so drop everything that is
  # definitely something else first -- quitting and hiding most of all -- and
  # only then take the best of what's left, most specific first: a plain
  # "show", then an "open" that names only the app, then one naming it at all,
  # then any "open", then anything naming Claude.  Without the tiers a later
  # release adding "Open Cowork" above "Show Claude" would win on menu order
  # alone.
  #
  # A wrong guess is worse than no guess, because clicking is all we can see:
  # the call succeeds when the app accepts the event, not when a window
  # appears, so a mis-click leaves nothing raised and nothing launched either.
  openEntry='
    def entries:
      .. | objects | select(.type == "(ia{sv}av)") | .data
      | select(((.[1].visible.data) != false) and ((.[1].enabled.data) != false))
      | select(((.[1].type.data) // "standard") != "separator")
      | select(((.[1]["children-display"].data) // "") != "submenu")
      | {id: .[0], label: ((.[1].label.data // "") | gsub("[_&]"; "") | ascii_downcase)};
    [entries]
    | map(select(.label | test("quit|exit|hide|close|sign ?out|log ?out|setting|preference|about|update|check for|developer|dev ?tools|console") | not))
    | (map(select(.label | test("show|restore|reopen")))
      + map(select(.label | test("^open( the)?( main)?( claude)?( app| window)?$")))
      + map(select((.label | test("open")) and (.label | test("claude"))))
      + map(select(.label | test("open")))
      + map(select(.label | test("claude"))))
    | first.id // empty
  '

  # The window belongs to the process that owns the surface, and that is the
  # one to recognize here -- not `app_id`, which is whatever the bundled
  # Electron build sets and can change with a release.  Match argv[0]'s
  # basename rather than the store path, so an instance still running from
  # before a switch is recognized too.  (Redirect stderr before opening the
  # file: the other order reports a vanished pid on the still-open stderr.)
  isClaude() {
    local argv0
    read -r -d "" argv0 2>/dev/null < "/proc/$1/cmdline" || return 1
    case ''${argv0##*/} in
      claude-desktop) return 0 ;;
      *) return 1 ;;
    esac
  }

  isRunning() {
    local cmdline pid
    for cmdline in /proc/[0-9]*/cmdline
    do
      pid="''${cmdline#/proc/}"
      isClaude "''${pid%/cmdline}" && return 0
    done
    return 1
  }

  # No guard on $SWAYSOCK: with no sway reachable this yields no windows,
  # which is the launch path anyway.
  focusWindow() {
    local window
    for window in $($swaymsg -t get_tree 2>/dev/null | $jq -r "$windows")
    do
      if isClaude "''${window%%:*}"
      then
        $swaymsg "[con_id=''${window##*:}] focus" >/dev/null && return 0
      fi
    done
    return 1
  }

  # Walk the tray items waybar knows about, find the one owned by Claude, and
  # click the entry that reopens the window.  The spec's own primary action,
  # `Activate`, would be the obvious thing to call instead, but Electron puts
  # the icon up through libayatana-appindicator, which serves the menu and
  # answers `Activate` with an error -- so it's only worth a try afterwards.
  openFromTray() {
    local item service path pid menu entry
    for item in $($busctl --user --json=short get-property \
      org.kde.StatusNotifierWatcher /StatusNotifierWatcher \
      org.kde.StatusNotifierWatcher RegisteredStatusNotifierItems 2>/dev/null \
      | $jq -r '.data[]?')
    do
      service="''${item%%/*}"
      case $item in
        */*) path="/''${item#*/}" ;;
        *) path=/StatusNotifierItem ;;
      esac

      pid=$($busctl --user --json=short call \
        org.freedesktop.DBus /org/freedesktop/DBus org.freedesktop.DBus \
        GetConnectionUnixProcessID s "$service" 2>/dev/null \
        | $jq -r '.data[0]? // empty')
      $test "$pid" || continue
      isClaude "$pid" || continue

      menu=$($busctl --user --json=short get-property "$service" "$path" \
        org.kde.StatusNotifierItem Menu 2>/dev/null | $jq -r '.data? // empty')

      if $test "$menu"
      then
        # Menus are commonly filled in only when they're about to be shown.
        $busctl --user call "$service" "$menu" \
          com.canonical.dbusmenu AboutToShow i 0 >/dev/null 2>&1

        # Depth 1, so only the entries a click can reach: a submenu's children
        # may not be filled in until its own AboutToShow, which we never send.
        # No property names, which the spec reads as all of them -- asking for
        # `label` alone would leave the filters above with nothing to test.
        entry=$($busctl --user --json=short call "$service" "$menu" \
          com.canonical.dbusmenu GetLayout iias -- 0 1 0 2>/dev/null \
          | $jq -r "$openEntry")

        if $test "$entry" && $busctl --user call "$service" "$menu" \
          com.canonical.dbusmenu Event isvu -- \
          "$entry" clicked s "" "$($date +%s)" >/dev/null 2>&1
        then
          return 0
        fi
      fi

      $busctl --user call "$service" "$path" \
        org.kde.StatusNotifierItem Activate ii 0 0 >/dev/null 2>&1 && return 0
    done
    return 1
  }

  # Arguments are a deep link for the running instance to open, which only
  # launching can hand over, so a bare invocation is the only one answered
  # with a raise.
  if $test $# -eq 0
  then
    focusWindow && exit
    openFromTray && exit

    # A copy still starting up has no window and no tray icon yet, which is
    # what a double-tapped launcher key looks like from here.  Wait for it
    # rather than answering with the second copy this exists to avoid --
    # polling the window, since a copy that is starting maps one rather than
    # stopping at the tray, and leaving as soon as it does.
    if isRunning
    then
      for _ in 1 2 3 4 5 6 7 8
      do
        $sleep 0.5
        focusWindow && exit
      done
      openFromTray && exit

      # Launching is still better than doing nothing, but say so.
      $notifySend -i claude-desktop "Claude is already running" \
        "Couldn't reach its window or tray icon -- starting another copy."
    fi
  fi

  exec $claudeDesktop "$@"
''
