{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.colorTheme;
  removeOctothorpe = builtins.replaceStrings ["#"] [""];

  # Every variant's files, under a directory named for the variant.
  variantsDir = "${config.xdg.dataHome}/color-theme";

  liveOptions = {
    options = {
      files = lib.mkOption {
        description = ''
          Files that follow the desktop's light/dark preference.  Each is
          rendered from every variant's palette, as text or as a file, and read
          through `link`.
        '';
        type = lib.types.attrsOf (lib.types.functionTo (lib.types.either lib.types.str lib.types.package));
        default = {};
      };

      link = lib.mkOption {
        description = ''
          The directory holding the variant the desktop shows, `files` by
          name.  It is the startup variant until `follow` moves it.
        '';
        type = lib.types.str;
        readOnly = true;
        default = "${config.xdg.stateHome}/color-theme/current";
      };

      follow = lib.mkEnableOption ''
        pointing `link` at the variant the desktop's settings portal names, for
        as long as the graphical session runs
      '';

      onSwitch = lib.mkOption {
        description = ''
          Shell run once `follow` has moved `link`, given the new variant's
          palette: for running programs that can be told to reload.
        '';
        type = lib.types.attrsOf (lib.types.functionTo lib.types.lines);
        default = {};
      };
    };

    config = {
      files = {
        # The theme's own name, which emacs picks its theme by.
        name = palette: palette.name;

        ls-colors = palette:
          pkgs.runCommand "ls-colors" {} ''
            ${pkgs.coreutils}/bin/dircolors --sh ${palette.dircolors} \
              | sed -n "s/^LS_COLORS='\(.*\)';$/\1/p" > $out
          '';

        fzf = palette: "--color=${lib.concatStringsSep "," (lib.mapAttrsToList (name: color: "${name}:${color}") {
          fg = palette.foreground;
          bg = palette.background;
          hl = palette.bright;
          "fg+" = palette.highlightForeground;
          "bg+" = palette.highlightBackground;
          "hl+" = palette.bright;
          info = palette.bright;
          prompt = palette.selection;
          pointer = palette.selection;
          marker = palette.selection;
          spinner = palette.selection;
          header = palette.secondaryContent;
          gutter = palette.highlightBackground;
        })}\n";

        "btop.theme" = palette:
          lib.concatStrings (lib.mapAttrsToList (name: color: "theme[${name}]=\"${color}\"\n") {
            main_bg = palette.background;
            main_fg = palette.foreground;
            title = palette.highlightForeground;
            hi_fg = palette.warn;
            selected_bg = palette.highlightBackground;
            selected_fg = palette.warn;
            inactive_fg = palette.secondaryContent;
            proc_misc = palette.green;
            cpu_box = palette.secondaryContent;
            mem_box = palette.secondaryContent;
            net_box = palette.secondaryContent;
            proc_box = palette.secondaryContent;
            div_line = palette.secondaryContent;
            temp_start = palette.blue;
            temp_mid = palette.lightPurple;
            temp_end = palette.red;
            cpu_start = palette.green;
            cpu_mid = palette.yellow;
            cpu_end = palette.lightRed;
            # One color each: btop takes a missing `_mid` from its default theme.
            free_start = palette.green;
            free_mid = "";
            free_end = palette.green;
            cached_start = palette.blue;
            cached_mid = "";
            cached_end = palette.blue;
            available_start = palette.yellow;
            available_mid = "";
            available_end = palette.yellow;
            used_start = palette.red;
            used_mid = "";
            used_end = palette.red;
            download_start = palette.lightPurple;
            download_mid = "";
            download_end = palette.lightPurple;
            upload_start = palette.purple;
            upload_mid = "";
            upload_end = palette.purple;
          });

        # The rest of imv's config comes along: it reads one file.
        imv = palette:
          lib.generators.toINI {} (lib.recursiveUpdate config.programs.imv.settings {
            options.background = removeOctothorpe palette.background;
          });
      };

      onSwitch = {
        # btop reloads its config, theme and all, on SIGUSR2.
        btop = lib.mkIf config.programs.btop.enable (_: ''
          ${pkgs.procps}/bin/pkill -USR2 -u "$(id -u)" -x btop
        '');

        # Each running imv, through its IPC socket.
        imv = lib.mkIf config.programs.imv.enable (palette: ''
          for socket in "''${XDG_RUNTIME_DIR:-/tmp}"/imv-*.sock; do
            [ -S "$socket" ] || continue
            pid=''${socket##*/imv-}
            ${config.programs.imv.package}/bin/imv-msg "''${pid%.sock}" \
              "background ${removeOctothorpe palette.background}" || true
          done
        '');
      };
    };
  };

  # `files.<name>`, rendered for one variant.
  render = variant: palette: name: content: let
    rendered = content palette;
  in {
    name = "color-theme/${variant}/${name}";
    value =
      if lib.isDerivation rendered
      then {source = rendered;}
      else {text = rendered;};
  };

  kittyColors = palette: ''
    foreground ${palette.foreground}
    background ${palette.background}
    cursor ${palette.foreground}
    cursor_text_color ${palette.background}
    url_color ${palette.selection}
    color0 ${palette.black}
    color1 ${palette.red}
    color2 ${palette.green}
    color3 ${palette.yellow}
    color4 ${palette.blue}
    color5 ${palette.purple}
    color6 ${palette.cyan}
    color7 ${palette.lightGrey}
    color8 ${palette.grey}
    color9 ${palette.lightRed}
    color10 ${palette.lightGreen}
    color11 ${palette.lightYellow}
    color12 ${palette.lightBlue}
    color13 ${palette.lightPurple}
    color14 ${palette.lightCyan}
    color15 ${palette.white}
  '';

  # `onSwitch`, as a `case` arm per variant.  One failing doesn't stop the
  # rest.
  switchCases = lib.concatStrings (lib.mapAttrsToList (variant: palette: ''
      ${variant})
      ${lib.concatMapStrings (hook: "(\n${hook palette}\n) || true\n") (lib.attrValues cfg.onSwitch)};;
    '')
    cfg.variants);

  # Watches the settings portal -- domicile's, on a domicile desk -- and points
  # `link` at the variant it names.  Nothing points it back when the session
  # ends: the next one starts by reading the portal again, and a restart
  # shouldn't flash the startup variant at what's open.
  follow = pkgs.writeShellApplication {
    name = "color-theme-follow";
    runtimeInputs = [pkgs.coreutils pkgs.gnugrep (lib.getBin pkgs.glib)];
    text = ''
      variants=${lib.escapeShellArg variantsDir}
      link=${lib.escapeShellArg cfg.link}

      switched() {
        case "$1" in
        ${switchCases}
        esac
      }

      point() {
        if [ "$(readlink "$link")" != "$variants/$1" ]; then
          mkdir -p "$(dirname "$link")"
          ln -sfn "$variants/$1" "$link.new"
          mv -T "$link.new" "$link"
          switched "$1"
        fi
      }

      # The portal's `color-scheme`: 1 is dark, 2 light, anything else no
      # preference.
      scheme() {
        case "$(grep -oE 'uint32 [0-9]+' | grep -oE '[0-9]+$' || true)" in
          1) point dark ;;
          2) point light ;;
          *) point ${cfg.variant} ;;
        esac
      }

      portal=(--session --dest org.freedesktop.portal.Desktop --object-path /org/freedesktop/portal/desktop)

      # Read once the monitor is up, so a change between the two isn't lost, and
      # again whenever the portal comes back.  A read that keeps failing -- a
      # backend not up yet -- leaves the link where it is rather than guess.
      gdbus monitor "''${portal[@]}" | while IFS= read -r line; do
        case "$line" in
          "Monitoring signals"* | "The name "*" is owned by "*)
            for _ in 1 2 3 4 5; do
              if value=$(gdbus call "''${portal[@]}" \
                --method org.freedesktop.portal.Settings.ReadOne \
                org.freedesktop.appearance color-scheme 2>/dev/null); then
                scheme <<<"$value"
                break
              fi
              sleep 0.5
            done
            ;;
          *"SettingChanged ('org.freedesktop.appearance', 'color-scheme',"*)
            scheme <<<"$line"
            ;;
        esac
      done
    '';
  };
in {
  options.colorTheme = lib.mkOption {
    type = lib.types.nullOr (pkgs.callPackage ../../lib/type/colorTheme.nix {
      modules = [liveOptions];
    });
    default = null;
  };

  config = lib.mkIf (cfg != null) {
    xdg.dataFile = lib.listToAttrs (
      lib.concatLists (lib.mapAttrsToList (
          variant: palette: lib.mapAttrsToList (render variant palette) cfg.files
        )
        cfg.variants)
    );

    # With `follow`, stays where it, or an earlier generation, put it; a link
    # to a variant that's gone falls back to the startup one.  Without, it is
    # always the startup one.
    home.activation.colorThemeLink = let
      link = lib.escapeShellArg cfg.link;
      startup = lib.escapeShellArg "${variantsDir}/${cfg.variant}";
    in
      lib.hm.dag.entryAfter ["linkGeneration"] ''
        if ${
          if cfg.follow
          then "[ ! -e ${link} ]"
          else "[ \"$(readlink ${link})\" != ${startup} ]"
        }; then
          run mkdir -p ${lib.escapeShellArg (dirOf cfg.link)}
          run ln -sfn ${startup} ${link}
        fi
      '';

    systemd.user.services.color-theme-follow = lib.mkIf cfg.follow {
      Unit = {
        Description = "Follow the desktop's light/dark preference";
        PartOf = ["graphical-session.target"];
        After = ["graphical-session.target"];
      };
      Service = {
        ExecStart = "${follow}/bin/color-theme-follow";
        Restart = "on-failure";
      };
      Install.WantedBy = ["graphical-session.target"];
    };

    # The login's environment.  fzf reads its file afresh each run, and shells
    # read `LS_COLORS` again before each prompt, below.
    home.sessionVariables = {
      LS_COLORS = "$(cat ${cfg.link}/ls-colors)";
      FZF_DEFAULT_OPTS_FILE = lib.mkIf config.programs.fzf.enable "${cfg.link}/fzf";
    };

    xdg.configFile = lib.mkMerge [
      # kitty follows the portal itself.
      (lib.mkIf config.programs.kitty.enable {
        "kitty/dark-theme.auto.conf".text = kittyColors cfg.variants.dark;
        "kitty/light-theme.auto.conf".text = kittyColors cfg.variants.light;
        "kitty/no-preference-theme.auto.conf".text = kittyColors cfg;
      })
      (lib.mkIf config.programs.btop.enable {
        "btop/themes/color-theme.theme".source = config.lib.file.mkOutOfStoreSymlink "${cfg.link}/btop.theme";
      })
      (lib.mkIf config.programs.imv.enable {
        "imv/config".source = lib.mkForce (config.lib.file.mkOutOfStoreSymlink "${cfg.link}/imv");
      })
    ];

    programs = {
      # `read`, a builtin, so a prompt doesn't fork for it.
      bash.initExtra = ''
        __color_theme_ls_colors() {
          { IFS= read -r LS_COLORS < ${lib.escapeShellArg "${cfg.link}/ls-colors"}; } 2>/dev/null
          export LS_COLORS
        }
        PROMPT_COMMAND="__color_theme_ls_colors''${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
      '';
      nushell.extraConfig = ''
        $env.config.hooks.pre_prompt = (
          $env.config.hooks.pre_prompt? | default [] | append {||
            $env.LS_COLORS = (try { open --raw "${cfg.link}/ls-colors" | str trim } catch { $env.LS_COLORS? | default "" })
          }
        )
      '';

      # btop only takes a theme by name, from its themes directories.
      btop.settings.color_theme = "color-theme";
      emacs.emacs-rc.colorThemeLink = cfg.link;

      termite = {
        backgroundColor = cfg.background;
        foregroundColor = cfg.foreground;
        hintsActiveBackgroundColor = cfg.selection;
        hintsActiveForegroundColor = cfg.background;
        hintsBackgroundColor = cfg.highlightBackground;
        hintsForegroundColor = cfg.highlightForeground;
        hintsBorderColor = cfg.highlightForeground;
        cursorColor = cfg.foreground;
        cursorForegroundColor = cfg.background;
        colorsExtra = ''
          color0 = ${cfg.black}
          color1 = ${cfg.red}
          color2 = ${cfg.green}
          color3 = ${cfg.yellow}
          color4 = ${cfg.blue}
          color5 = ${cfg.purple}
          color6 = ${cfg.cyan}
          color7 = ${cfg.lightGrey}
          color8 = ${cfg.grey}
          color9 = ${cfg.lightRed}
          color10 = ${cfg.lightGreen}
          color11 = ${cfg.lightYellow}
          color12 = ${cfg.lightBlue}
          color13 = ${cfg.lightPurple}
          color14 = ${cfg.lightCyan}
          color15 = ${cfg.white}
        '';
      };
      zathura.options = {
        default-bg = cfg.highlightBackground;
        default-fg = cfg.highlightForeground;
        highlight-color = cfg.highlightBackground;
        highlight-active-color = cfg.selection;
        completion-bg = cfg.highlightBackground;
        completion-fg = cfg.secondaryContent;
        completion-group-bg = cfg.highlightForeground;
        completion-group-fg = cfg.highlightBackground;
        completion-highlight-bg = cfg.selection;
        completion-highlight-fg = cfg.highlightBackground;
        inputbar-bg = cfg.background;
        inputbar-fg = cfg.foreground;
        statusbar-bg = cfg.highlightBackground;
        statusbar-fg = cfg.highlightForeground;
        notification-bg = cfg.background;
        notification-fg = cfg.foreground;
        notification-error-bg = cfg.warn;
        notification-error-fg = cfg.background;
        notification-warning-bg = cfg.urgent;
        notification-warning-fg = cfg.background;
        recolor-darkcolor = cfg.foreground;
        recolor-lightcolor = cfg.background;
        render-loading-bg = cfg.highlightBackground;
        render-loading-fg = cfg.highlightForeground;
      };

      swaylock.settings = {
        color = removeOctothorpe cfg.background;
        text-caps-lock-color = removeOctothorpe cfg.background;
        text-ver-color = removeOctothorpe cfg.background;
        text-wrong-color = removeOctothorpe cfg.background;
        bs-hl-color = removeOctothorpe cfg.warn;
        caps-lock-bs-hl-color = removeOctothorpe cfg.urgent;
        caps-lock-key-hl-color = removeOctothorpe cfg.selection;
        inside-color = "00000000";
        inside-clear-color = removeOctothorpe cfg.warn + "D0";
        inside-caps-lock-color = removeOctothorpe cfg.urgent + "D0";
        inside-ver-color = removeOctothorpe cfg.selection + "D0";
        inside-wrong-color = removeOctothorpe cfg.urgent + "D0";
        ring-color = removeOctothorpe cfg.bright;
        ring-clear-color = removeOctothorpe cfg.warn;
        ring-caps-lock-color = removeOctothorpe cfg.urgent;
        ring-ver-color = removeOctothorpe cfg.selection;
        ring-wrong-color = removeOctothorpe cfg.urgent;
        line-color = removeOctothorpe cfg.highlightBackground;
        line-clear-color = removeOctothorpe cfg.highlightBackground;
        line-caps-lock-color = removeOctothorpe cfg.highlightBackground;
        line-ver-color = removeOctothorpe cfg.highlightBackground;
        line-wrong-color = removeOctothorpe cfg.highlightBackground;
        separator-color = removeOctothorpe cfg.highlightBackground;
      };
    };

    services.mako.settings = {
      background-color = "${cfg.foreground}E0";
      text-color = cfg.background;
      border-color = cfg.highlightForeground;
      progress-color = "source ${cfg.bright}";
    };

    wayland.windowManager.sway.config.colors = {
      inherit (cfg) background;

      focused = {
        border = cfg.selection;
        background = cfg.selection;
        text = cfg.background;
        indicator = cfg.bright;
        childBorder = cfg.selection;
      };

      focusedInactive = {
        border = cfg.highlightForeground;
        background = cfg.highlightForeground;
        text = cfg.highlightBackground;
        indicator = cfg.highlightForeground;
        childBorder = cfg.highlightForeground;
      };

      unfocused = {
        border = cfg.highlightForeground;
        background = cfg.highlightBackground;
        text = cfg.highlightForeground;
        indicator = cfg.highlightBackground;
        childBorder = cfg.highlightBackground;
      };

      urgent = {
        border = cfg.urgent;
        background = cfg.urgent;
        text = cfg.background;
        indicator = cfg.urgent;
        childBorder = cfg.urgent;
      };
    };
  };
}
