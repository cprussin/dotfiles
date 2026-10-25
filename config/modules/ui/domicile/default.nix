# This desk, for domicile.  The first output profile whose monitors are exactly
# what is plugged in wins, re-matched on every hotplug.
#
# Names are the panel's EDID string as domicile builds it: the make is the
# three-letter PNP id ("DEL", not "Dell Inc.") and a missing serial is left
# out.  A name that matches nothing is not an error -- the desk just comes up
# unplaced.
#
# Positions assume every monitor comes up at its native mode.  A domicile
# profile has no mode field, so the mode arrives with the monitor and the
# compositor divides it by the scale.
#
# If a screen comes up upside down, the fix is domicile's cover-the-window.ts.
{
  config,
  lib,
  pkgs,
  ...
}: let
  # Physical pixels and the density each panel is readable at.  `logical` is
  # what the desktop is laid out in, left fractional so that positions -- which
  # are sums of these -- don't drift a pixel per monitor across the desk.
  panel = scale: width: height: {
    inherit scale;
    logical = {
      width = width / scale;
      height = height / scale;
    };
  };

  laptop = panel 1.5 2880 1920;
  portable = panel 1.4 3840 2160;
  curved = panel 1.0 3440 1440;
  # The three on the desk are the same monitor three times over.
  external = panel 1.2 3840 2160;

  names = {
    laptop = "BOE NE135A1M-NY1";
    curved = "GSM LG ULTRAWIDE 0x0001E368";
    left = "DEL DELL U3219Q 2ZLS413";
    center = "DEL DELL U3219Q G3MS413";
    right = "DEL DELL U3219Q H8KF413";
    # The make in full is "Audio Processing Technology  Ltd"; the double space
    # is pnp.ids' own, which is what makes the model "Monitor".
    portable = "APT Monitor demoset-1";
  };

  # Rounded outward: a monitor a pixel further out
  # leaves a gap nobody sees, where one a pixel short overlaps its neighbour --
  # and domicile has no mirroring, so an overlap is two screens claiming one
  # patch of desktop.
  at = x: y: monitor: {
    inherit (monitor) scale;
    position = [(builtins.ceil x) (builtins.ceil y)];
  };

  # A quarter turn.
  sideways = x: y: monitor: (at x y monitor) // {transform = "rotate-270";};

  # What a monitor measures once it is stood on its side.
  turnedWidth = monitor: monitor.logical.height;
  turnedHeight = monitor: monitor.logical.width;

  profile = name: displays: {
    inherit name;
    displays =
      lib.mapAttrsToList
      (key: placement: placement // {display = names.${key};})
      displays;
  };

  hm = config.primary-user.home-manager;

  inTerminal = name: bin: "${hm.default-terminal.bin} --title ${name} --class ${name} --name ${name} ${bin}";

  # `package` with `flags` added to `bin`, in its desktop entries and D-Bus
  # services too, so a link that starts it (slack://, tg://, element://) runs
  # it as the launcher does.  Telegram's entry is `DBusActivatable`, so GIO
  # starts it through its service, not the entry's `Exec`.  Fails the build if
  # an `Exec` names some other program.
  withFlags = package: bin: flags:
    pkgs.symlinkJoin {
      name = "${package.name}-with-flags";
      paths = [package];
      nativeBuildInputs = [pkgs.makeWrapper];
      postBuild = ''
        rm "$out/bin/${bin}"
        makeWrapper "${package}/bin/${bin}" "$out/bin/${bin}" --add-flags "${flags}"
        for file in "$out"/share/applications/*.desktop "$out"/share/dbus-1/services/*.service
        do
          [ -e "$file" ] || continue
          original="$(readlink -f "$file")"
          rm "$file"
          sed -E 's#^(Try)?Exec=([^ ]*/)?${bin}( |$)#\1Exec='"$out"'/bin/${bin}\3#' "$original" > "$file"
          if grep -E '^Exec=' "$file" | grep -v -q -F "$out/bin/${bin}"
          then
            echo "an Exec in $original doesn't run ${bin}" >&2
            exit 1
          fi
        done
      '';
    };

  element = withFlags pkgs.element-desktop "element-desktop" "--ozone-platform-hint=auto";
  telegram-desktop = withFlags pkgs.telegram-desktop "Telegram" "--ozone-platform-hint=auto -g warn";
  slack-desktop = withFlags pkgs.slack "slack" "--ozone-platform-hint=auto -g warn";

  matrix = "${element}/bin/element-desktop";
  telegram = "${telegram-desktop}/bin/Telegram";
  slack = "${slack-desktop}/bin/slack";
  # The programs domicile's launcher offers, by desktop file ID
  # `domicile-<app>.desktop`, beside the History, Settings, Screenshot, Shutdown
  # and Reboot entries domicile ships itself.  What opens a URL is a bookmark
  # instead, which domicile opens as a page of its own rather than
  # handing to a browser.  `windowClass` is the app id of the window an app
  # opens, which domicile's title bars look its icon up by; `package` instead
  # takes it from that package's own desktop entry at build time, for apps
  # whose app id isn't known here.
  apps = {
    agenda = {
      name = "Agenda";
      exec = pkgs.writeShellScript "agenda" "exec ${pkgs.emacs}/bin/emacsclient -c -e '(org-agenda nil \"a\")'";
    };
    bluetooth = {
      name = "Bluetooth";
      exec = inTerminal "bluetooth" "${pkgs.bluetuith}/bin/bluetuith";
      windowClass = "bluetooth";
    };
    btop = {
      name = "btop";
      exec = inTerminal "btop" "${pkgs.btop}/bin/btop";
      windowClass = "btop";
    };
    chatgpt = {
      name = "ChatGPT";
      exec = "${pkgs.chatgpt}/bin/chatgpt";
      package = pkgs.chatgpt;
    };
    claude = {
      name = "Claude";
      exec = "${pkgs.claude-desktop}/bin/claude-desktop";
      package = pkgs.claude-desktop;
    };
    # SMS is the bar's Google Messages item, in ./shell.tsx.
    comms = {
      name = "Comms";
      exec = pkgs.writeShellScript "comms" ''
        ${matrix} &
        ${telegram} &
        ${slack} &
      '';
    };
    crux = {
      name = "crux";
      exec = inTerminal "crux" "${pkgs.openssh}/bin/ssh -t crux load-session";
      windowClass = "crux";
    };
    emacs = {
      name = "Emacs";
      exec = "${pkgs.emacs}/bin/emacsclient -c";
      windowClass = "emacs";
    };
    journal = {
      name = "Journal";
      exec = inTerminal "journal" "sudo ${pkgs.systemd}/bin/journalctl -alf";
      windowClass = "journal";
    };
    matrix = {
      name = "Matrix";
      exec = matrix;
      windowClass = "Element";
    };
    mixer = {
      name = "Mixer";
      exec = "${pkgs.pavucontrol}/bin/pavucontrol";
      windowClass = "org.pulseaudio.pavucontrol";
    };
    slack = {
      name = "Slack";
      exec = slack;
      windowClass = "Slack";
    };
    telegram = {
      name = "Telegram";
      exec = telegram;
      windowClass = "org.telegram.desktop";
    };
    terminal = {
      name = "Terminal";
      exec = hm.default-terminal.bin;
      windowClass = "kitty";
    };
    tor-browser = {
      name = "Tor Browser";
      exec = "${pkgs.tor-browser}/bin/tor-browser";
      windowClass = "Tor Browser";
    };
  };

  # `desktop` with the `StartupWMClass` `package`'s desktop entries name, or
  # the id of its one entry if none does -- the app id its window takes.
  withPackageClass = app: package: desktop:
    pkgs.runCommand "domicile-${app}.desktop" {} ''
      entries=(${package}/share/applications/*.desktop)
      class="$(sed -n 's/^StartupWMClass=//p' "''${entries[@]}" | sed -n 1p)"
      if [ -z "$class" ] && [ "''${#entries[@]}" -eq 1 ] && [ -e "''${entries[0]}" ]
      then
        class="$(basename "''${entries[0]}" .desktop)"
      fi
      if [ -z "$class" ]
      then
        echo "no app id in ${package}'s desktop entries: ''${entries[*]}" >&2
        exit 1
      fi
      cat ${desktop} > $out
      echo "StartupWMClass=$class" >> $out
    '';

  # A Google bookmark on the personal account.
  personal = url: "${url}?authuser=connor@prussin.net";

  # A Google bookmark once per account: `<name> - PrussinNet` and
  # `<name> - Meat Proxy Labs`.
  google = name: url: {
    "${name} - PrussinNet".url = personal url;
    "${name} - Meat Proxy Labs".url = "${url}?authuser=connor@meatproxylabs.com";
  };

  # manganese's launcher, in ./shell.tsx: `apps` above and nothing a package
  # happened to install -- every desktop entry is left out, then `domicile-*`
  # taken back.
  applications = {
    omit = ["*" "!domicile-*"];
    bookmarks = lib.mapAttrsToList (name: bookmark: bookmark // {inherit name;}) (
      google "Calendar" "https://calendar.google.com"
      // google "Email" "https://mail.google.com"
      // google "Google Drive" "https://drive.google.com"
      // {
        "Credit Cards".url = personal "https://docs.google.com/spreadsheets/d/1Y8xind-5nMe9bezMFmk__CQdkSBd7FPupt1NkdKDLUE";
        "SOTD".url = personal "https://docs.google.com/spreadsheets/d/168kHAuFM2bOHaQvyzkbWBF4206jV5bXpg0ubT3fSSJk";
        "Eyes".url = "https://eyes.internal.prussin.net";
        "Home".url = "https://home-assistant.internal.prussin.net";
        "Photos".url = "https://photos.internal.prussin.net";
        "Syncthing".url = "http://localhost:8384";
      }
    );
  };

  keyboard = hm.keymap;

  # The home-manager `keymap` option, then plain us to switch to.  The desk
  # starts on the first; `options` goes across as the list it is.
  keymaps = lib.optionals (keyboard != null) (let
    own = {
      name =
        if keyboard.variant == ""
        then keyboard.layout
        else keyboard.variant;
      xkb_layout = keyboard.layout;
      xkb_variant = keyboard.variant;
      xkb_options = keyboard.options;
    };
  in
    [own] ++ lib.optional (own.name != "us") {name = "us";});

  # A launcher app between `keymaps`, for someone else at the keyboard.  The
  # bar's keymap item shows while on us.
  toggleKeymap = lib.optionalAttrs (lib.length keymaps > 1) {
    toggle-keymap = {
      name = "Toggle keymap";
      exec = "${hm.programs.domicile.package}/bin/domicile send-shell keymap next";
    };
  };

  # domicile's theme, out of the same `colorTheme` everything else here reads.
  # Names are `<family>/<variant>` and the variants are exactly domicile's two.
  # Both the attrset and its `name` are nullable; either null leaves domicile's
  # own `theme.mode` option at its default (`dark`).
  themeMode =
    if hm.colorTheme == null || hm.colorTheme.name == null
    then null
    else lib.last (lib.splitString "/" hm.colorTheme.name);
in {
  primary-user.home-manager = {
    imports = [config.flake-inputs.domicile.homeManagerModules.default];

    # Named here because the domicile module adds its package at the normal
    # priority this repo's `lib.mkForce` discards -- the trap ui/xdg-portal
    # documents.  Without it `domicile` is off PATH, and its `.portal` file and
    # `domicile-portals.conf` (the module's portal routing) never reach the
    # profile.
    #
    # xdg-utils is the system `xdg-open` that domicile's own `xdg-open` hands
    # everything but a web link to.
    #
    # The chat apps' desktop entries handle their links (slack://, tg://,
    # element://), sign-in links among them, so they are in the profile and
    # not only behind `apps` above.  The launcher omits them.  ChatGPT stays
    # out: it writes its own entry, which claims http/https, into
    # ~/.local/share/applications, and with `chatgpt` on PATH that entry would
    # open every web link.
    home.packages = lib.mkForce (
      [
        element
        pkgs.xdg-utils
        slack-desktop
        telegram-desktop
      ]
      ++ lib.optional hm.programs.domicile.enable hm.programs.domicile.finalPackage
    );

    xdg = {
      # A read-only store symlink at ~/.config/mimeapps.list (and the
      # deprecated copy under ~/.local/share/applications), so defaults are set
      # here, not at runtime.  It names no web browser, so domicile's own
      # default answers web links.  Apps that make themselves the default
      # (`xdg-mime default`, Electron's `setAsDefaultProtocolClient`) replace
      # the symlink with a plain file; `force` overwrites that at the next
      # activation rather than failing on it.
      mimeApps.enable = true;
      configFile."mimeapps.list".force = true;

      # Each app's icon and the picture its launcher preview shows are
      # ./launcher/<app>.svg and ./launcher/<app>-preview.svg.
      dataFile =
        {"applications/mimeapps.list".force = true;}
        // lib.mapAttrs' (app: entry:
          lib.nameValuePair "applications/domicile-${app}.desktop" {
            source = let
              desktop = "${pkgs.makeDesktopItem {
                name = "domicile-${app}";
                desktopName = entry.name;
                exec = "${entry.exec}";
                icon = "${./launcher + "/${app}.svg"}";
                extraConfig."X-Domicile-Preview" = "${./launcher + "/${app}-preview.svg"}";
                startupWMClass = entry.windowClass or null;
                terminal = false;
              }}/share/applications/domicile-${app}.desktop";
            in
              if entry ? package
              then withPackageClass app entry.package desktop
              else desktop;
          })
        (apps // toggleKeymap);

      # The shell ./shell.tsx, where `programs.domicile.settings.shell` below
      # names it, with the terminal's path and the launcher's `applications`
      # in.
      configFile."domicile/shell.tsx".source = pkgs.replaceVars ./shell.tsx {
        terminal = hm.default-terminal.bin;
        applications = builtins.toJSON applications;
      };
    };

    programs.domicile = {
      # `mkDefault` so a host on this profile can turn it off: a bare `true`
      # would make its `false` a conflict rather than an override.
      enable = lib.mkDefault true;
      settings = {
        # What the desk COMES UP on: domicile's own toggle changes the live
        # theme without writing back, and the next rebuild restates this.
        #
        # The pointer is the one every other toolkit here draws: ui/theme's
        # cursor, through home-manager's `pointerCursor`, with its light-mode
        # set when it has one.
        theme =
          lib.optionalAttrs (themeMode != null) {mode = themeMode;}
          // lib.optionalAttrs (hm.home.pointerCursor != null) {
            cursor_theme =
              if hm.cursorTheme.lightName == null
              then hm.home.pointerCursor.name
              else {
                dark = hm.home.pointerCursor.name;
                light = hm.cursorTheme.lightName;
              };
            cursor_size = hm.home.pointerCursor.size;
          }
          # Gmail has no dark theme that follows the system, so the browser
          # draws it dark while the desk is.
          // {force_dark = ["mail.google.com"];};

        # The launcher's file index: every top-level entry of ~ but its
        # dotfiles, one level into Library, Notes and Projects, and all of
        # Scratch.  Dotfiles below ~ are kept -- a stated list replaces
        # domicile's hidden-file default, and `.*` stops at a `/`.
        files.omit = [
          ".*"
          "*/*"
          "!{Library,Notes,Projects,Scratch}/*"
          "{Library,Notes,Projects}/*/*"
        ];

        # This desk's emacs daemon, up before the first frame asks for it.
        startup.commands =
          lib.optional hm.programs.emacs.enable ["${pkgs.emacs}/bin/emacsclient" "-e" "t"];

        input = lib.optionalAttrs (keymaps != []) {
          keyboard.keymaps = keymaps;
        };

        # The keymap item only while switched off the desk's own keymap.
        shell_options.keymap.hideDefault = true;

        output.profiles = [
          # The lid open and nothing plugged in.
          (profile "laptop-only" {
            laptop = at 0 0 laptop;
          })

          # The travel monitor, to the right of the laptop.
          (profile "portable-panel" {
            laptop = at 0 0 laptop;
            portable = at laptop.logical.width 0 portable;
          })

          # One monitor on the desk, the laptop centred underneath it.
          (profile "home-office-center" {
            laptop =
              at
              ((external.logical.width - laptop.logical.width) / 2)
              external.logical.height
              laptop;
            center = at 0 0 external;
          })

          # Two of the three on their sides, the laptop bottom-aligned to their
          # left: its top edge is how tall they are turned, less its own height.
          (profile "home-office-right-two" {
            laptop = at 0 ((turnedHeight external) - laptop.logical.height) laptop;
            center = sideways laptop.logical.width 0 external;
            right = sideways (laptop.logical.width + (turnedWidth external)) 0 external;
          })

          # The full desk, lid shut.  The panel is still named: a profile
          # applies only to the exact set it names, and a shut panel is still
          # connected.
          (profile "home-office-full" {
            laptop = {enabled = false;};
            left = sideways 0 0 external;
            center = sideways (turnedWidth external) 0 external;
            right = sideways (2 * (turnedWidth external)) 0 external;
          })

          # The ultrawide, with the laptop centred underneath it.
          (profile "home-office-curved" {
            laptop =
              at
              ((curved.logical.width - laptop.logical.width) / 2)
              curved.logical.height
              laptop;
            curved = at 0 0 curved;
          })
        ];

        # The screens blank after ten idle minutes, and the desk locks thirty
        # seconds later: a key or the mouse in between wakes the screens
        # without the passphrase.
        idle = {
          blank_after_seconds = 10 * 60;
          lock_after_seconds = 10 * 60 + 30;
        };

        # The shell, and with it the keys: ./shell.tsx, which domicile builds
        # itself as the desk starts -- once, then from its cache until the file
        # changes.  An edit reaches the desk at the next start.
        shell = "${hm.xdg.configHome}/domicile/shell.tsx";
      };
    };
  };

  # The machine's half (domicile's NixOS module, imported in flake.nix): the
  # `domicile` PAM service, which the lock (Meta+Shift+Return, or the idle
  # timeout above) checks the password against by default, and the `domicile`
  # login session, which runs the config's ./shell.tsx.  Booting into it is
  # this line, not the module's.
  programs.domicile = {inherit (hm.programs.domicile) enable;};
  services.displayManager.defaultSession =
    lib.mkIf hm.programs.domicile.enable "domicile";

  # The module lets members of `domicile` lower nice to -10 and use realtime
  # priority 8, which the engine's frame and audio threads ask for.  Takes
  # effect at the next login.
  primary-user.extraGroups = lib.mkIf hm.programs.domicile.enable ["domicile"];
}
