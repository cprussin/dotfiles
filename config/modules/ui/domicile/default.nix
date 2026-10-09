# This desk, for domicile.  The sway half of it is
# config/modules/ui/kanshi/default.nix: same monitors, same arithmetic, and the
# same rule -- the first profile whose monitors are exactly what is plugged in
# wins, re-matched on every hotplug.
#
# Names are the panel's EDID string as domicile builds it, which differs from
# kanshi's in two ways: the make is the three-letter PNP id ("DEL", not "Dell
# Inc.") and a missing serial is left out rather than spelled "Unknown".  A
# name that matches nothing is not an error -- the desk just comes up unplaced.
#
# Positions assume every monitor comes up at its native mode.  kanshi pins one
# per output; a domicile profile has no mode field, so the mode arrives with
# the monitor and the compositor divides it by the scale.
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
    # kanshi says "Audio Processing Technology  Ltd Monitor demoset-1"; the
    # double space is pnp.ids' own, which is what makes the model "Monitor".
    portable = "APT Monitor demoset-1";
  };

  # Rounded outward, like the kanshi module: a monitor a pixel further out
  # leaves a gap nobody sees, where one a pixel short overlaps its neighbour --
  # and domicile has no mirroring, so an overlap is two screens claiming one
  # patch of desktop.
  at = x: y: monitor: {
    inherit (monitor) scale;
    position = [(builtins.ceil x) (builtins.ceil y)];
  };

  # A quarter turn, the same 270 the kanshi file writes for these three.
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

  # domicile's own yes/no dialog -- its Access portal, asked directly -- and
  # `cmd` only on a yes.  Esc, "Cancel" or no shell to ask all leave it be.
  confirm = name: title: subtitle: grant: cmd:
    pkgs.writeShellScript name ''
      reply=$(${pkgs.systemd}/bin/busctl --user --timeout=infinity call \
        org.freedesktop.impl.portal.desktop.domicile \
        /org/freedesktop/portal/desktop \
        org.freedesktop.impl.portal.Access AccessDialog 'osssssa{sv}' \
        "/org/freedesktop/portal/desktop/request/launcher/${name}_$$" \
        domicile-${name} "" \
        ${lib.escapeShellArg title} ${lib.escapeShellArg subtitle} "" \
        2 grant_label s ${lib.escapeShellArg grant} deny_label s Cancel)
      if [ "$reply" = 'ua{sv} 0 0' ]; then
        exec ${cmd}
      fi
    '';

  matrix = "${pkgs.element-desktop}/bin/element-desktop --ozone-platform-hint=auto";
  telegram = "${pkgs.telegram-desktop}/bin/Telegram --ozone-platform-hint=auto -g warn";
  slack = "${pkgs.slack}/bin/slack --ozone-platform-hint=auto -g warn";
  # The programs domicile's launcher offers, by desktop file ID
  # `domicile-<app>.desktop`: its own, and none of ui/launcher's commands --
  # though chatgpt-desktop and claude-desktop still come from that module's
  # overlays, which have to move before it goes.  What opens a URL is a bookmark instead, which
  # domicile opens as a page of its own rather than handing to a browser.
  apps = {
    agenda = {
      name = "Agenda";
      exec = pkgs.writeShellScript "agenda" "exec ${pkgs.emacs}/bin/emacsclient -c -e '(org-agenda nil \"a\")'";
    };
    bluetooth = {
      name = "Bluetooth";
      exec = inTerminal "bluetooth" "${pkgs.bluetuith}/bin/bluetuith";
    };
    btop = {
      name = "btop";
      exec = inTerminal "btop" "${pkgs.btop}/bin/btop";
    };
    chatgpt = {
      name = "ChatGPT";
      exec = "${pkgs.chatgpt-desktop}/bin/chatgpt";
    };
    claude = {
      name = "Claude";
      exec = "${pkgs.claude-desktop}/bin/claude-desktop";
    };
    # SMS is a bookmark, which no command can open.
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
    };
    emacs = {
      name = "Emacs";
      exec = "${pkgs.emacs}/bin/emacsclient -c";
    };
    journal = {
      name = "Journal";
      exec = inTerminal "journal" "sudo ${pkgs.systemd}/bin/journalctl -alf";
    };
    matrix = {
      name = "Matrix";
      exec = matrix;
    };
    mixer = {
      name = "Mixer";
      exec = "${pkgs.pavucontrol}/bin/pavucontrol";
    };
    reboot = {
      name = "Reboot";
      exec = confirm "reboot" "Reboot?" "Every open window closes." "Reboot" "${pkgs.systemd}/bin/systemctl reboot -i";
    };
    # The shell's picker, as Print: a monitor, a window or an area, saved
    # under ~/Scratch/Screenshots (XDG_PICTURES_DIR).
    screenshot = {
      name = "Screenshot";
      exec = "${hm.programs.domicile.finalPackage}/bin/domicile screenshot";
    };
    shutdown = {
      name = "Shutdown";
      exec = confirm "shutdown" "Shut down?" "Every open window closes." "Shut down" "${pkgs.systemd}/bin/systemctl poweroff -i";
    };
    slack = {
      name = "Slack";
      exec = slack;
    };
    telegram = {
      name = "Telegram";
      exec = telegram;
    };
    terminal = {
      name = "Terminal";
      exec = hm.default-terminal.bin;
    };
    tor-browser = {
      name = "Tor Browser";
      exec = "${pkgs.tor-browser}/bin/tor-browser";
    };
  };

  # A Google bookmark on the personal account.
  personal = url: "${url}?authuser=connor@prussin.net";

  # A Google bookmark once per account: `<name> - PrussinNet` and
  # `<name> - Meat Proxy Labs`.
  google = name: url: {
    "${name} - PrussinNet".url = personal url;
    "${name} - Meat Proxy Labs".url = "${url}?authuser=connor@meatproxylabs.com";
  };

  # manganese's launcher, in ./shell.ts: `apps` above and nothing a package
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
        "SMS".url = personal "https://messages.google.com/web/conversations";
        "SOTD".url = personal "https://docs.google.com/spreadsheets/d/168kHAuFM2bOHaQvyzkbWBF4206jV5bXpg0ubT3fSSJk";
        "Eyes".url = "https://eyes.internal.prussin.net";
        "Home".url = "https://home-assistant.internal.prussin.net";
        "Photos".url = "https://photos.internal.prussin.net";
        "Syncthing".url = "http://localhost:8384";
      }
    );
  };

  keyboard = hm.keymap;

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
    home.packages = lib.mkForce (
      lib.optional hm.programs.domicile.enable hm.programs.domicile.finalPackage
    );

    xdg = {
      # Each app's icon and the picture its launcher preview shows are
      # ./launcher/<app>.svg and ./launcher/<app>-preview.svg.
      dataFile = lib.mapAttrs' (app: entry:
        lib.nameValuePair "applications/domicile-${app}.desktop" {
          source = "${pkgs.makeDesktopItem {
            name = "domicile-${app}";
            desktopName = entry.name;
            exec = "${entry.exec}";
            icon = "${./launcher + "/${app}.svg"}";
            extraConfig."X-Domicile-Preview" = "${./launcher + "/${app}-preview.svg"}";
            terminal = false;
          }}/share/applications/domicile-${app}.desktop";
        })
      apps;

      # The shell ./shell.ts, where `programs.domicile.settings.shell` below
      # names it, with the terminal's path and the launcher's `applications`
      # in.
      configFile."domicile/shell.ts".source = pkgs.replaceVars ./shell.ts {
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
        theme = lib.optionalAttrs (themeMode != null) {mode = themeMode;};

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

        # This desk's emacs daemon, up before the first frame asks for it --
        # the same one ui/emacs has sway start.
        startup.commands =
          lib.optional hm.programs.emacs.enable ["${pkgs.emacs}/bin/emacsclient" "-e" "t"];

        # The same option sway reads, not a copy of it: a host setting only one
        # of the two would give sway one layout and domicile another with
        # nothing to say so.  `options` goes across as the list it is.
        input = lib.optionalAttrs (keyboard != null) {
          keyboard = {
            xkb_layout = keyboard.layout;
            xkb_variant = keyboard.variant;
            xkb_options = keyboard.options;
          };
        };

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

        # Meta+Shift+Return locks the desk, checking the password against the
        # PAM service below.
        lock.pam_service = "domicile";

        # The shell, and with it the keys: ./shell.ts, which domicile builds
        # itself as the desk starts -- once, then from its cache until the file
        # changes.  An edit reaches the desk at the next start.
        shell = "${hm.xdg.configHome}/domicile/shell.ts";
      };
    };
  };

  # The machine's half (domicile's NixOS module, imported in flake.nix): the
  # `domicile` PAM service the lock above names, and the `domicile` login
  # session, which runs the config's ./shell.ts.  Booting into it is this line,
  # not the module's.
  programs.domicile = {inherit (hm.programs.domicile) enable;};
  services.displayManager.defaultSession =
    lib.mkIf hm.programs.domicile.enable "domicile";

  # The module lets members of `domicile` lower nice to -10 and use realtime
  # priority 8, which the engine's frame and audio threads ask for.  Takes
  # effect at the next login.
  primary-user.extraGroups = lib.mkIf hm.programs.domicile.enable ["domicile"];
}
