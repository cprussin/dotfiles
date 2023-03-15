{
  config,
  lib,
  ...
}: let
  cfg = config.programs.imv;
in {
  options.programs.imv = {
    enable = lib.mkEnableOption "imv";

    background = lib.mkOption {
      type = lib.types.str;
      description = ''
        Set the background in imv. Can either be a 6-digit hexadecimal color
        code, or 'checks' for a chequered background.
      '';
      default = "000000";
    };

    fullscreen = lib.mkOption {
      type = lib.types.bool;
      description = "Start imv fullscreen.";
      default = false;
    };

    width = lib.mkOption {
      type = lib.types.int;
      description = "Initial width of the imv window.";
      default = 1280;
    };

    height = lib.mkOption {
      type = lib.types.int;
      description = "Initial height of the imv window.";
      default = 720;
    };

    initialPan = lib.mkOption {
      type = lib.types.submodule {
        options = {
          x = lib.mkOption {
            type = lib.types.int;
            default = 50;
          };
          y = lib.mkOption {
            type = lib.types.int;
            default = 50;
          };
        };
      };
      description = ''
        Initial pan/focus position factor of the opened images. A value of 50
        represents the middle point of the image (50%).
      '';
      default = {
        x = 50;
        y = 50;
      };
    };

    listFilesAtExit = lib.mkOption {
      type = lib.types.bool;
      description = ''
        Print open files to stdout at exit, each on a separate line.
      '';
      default = false;
    };

    loopInput = lib.mkOption {
      type = lib.types.bool;
      description = "Return to first image after viewing the last one.";
      default = true;
    };

    overlay = lib.mkOption {
      type = lib.types.bool;
      description = "Start with the overlay visible.";
      default = false;
    };

    overlayText = lib.mkOption {
      type = lib.types.str;
      description = ''
        Use the given text as the overlay’s text. The provided text is shell
        expanded, so the output of commands can be used: $(ls) as can
        environment variables, including the ones accessible to imv’s exec
        command.
      '';
      default = "";
    };

    recursively = lib.mkOption {
      type = lib.types.bool;
      description = "Load input paths recursively.";
      default = false;
    };

    scalingMode = lib.mkOption {
      type = lib.types.enum ["none" "shrink" "full" "crop"];
      description = ''
        Set scaling mode to use.  'none' will show each image at its actual
        size.  'shrink' will scale down the image to fit inside the window.
        'full' will both scale up and scale down the image to fit perfectly
        inside the window.  'crop' will scale and crop the image to fill the
        window.
      '';
      default = "full";
    };

    slideshowDuration = lib.mkOption {
      type = lib.types.int;
      description = ''
        Start imv in slideshow mode, and set the amount of time to show each
        image for in seconds.
      '';
      default = 0;
    };

    suppressDefaultBinds = lib.mkOption {
      type = lib.types.bool;
      description = ''
        Disable imv’s built-in binds so they don’t conflict with custom ones.
      '';
      default = false;
    };

    titleText = lib.mkOption {
      type = lib.types.str;
      description = ''
        Use the given text as the window’s title. The provided text is shell
        expanded, so the output of commands can be used: $(ls) as can
        environment variables, including the ones accessible to imv’s exec
        command.
      '';
      default = "";
    };

    upscalingMethod = lib.mkOption {
      type = lib.types.enum ["linear" "nearest_neighbor"];
      description = "Use the specified method to upscale images.";
      default = "linear";
    };

    aliases = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      description = ''
        Allows aliases to be added for imv’s built-in commands. For example,
        `{ x = "close"; }` would add a x command that simply executes the close
        command. Any arguments provided to an alias are appended to the command
        configured by the alias.
      '';
      default = {};
    };

    binds = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      description = ''
        Allows custom key bindings to be added to imv.  The attr key is the key
        combination to bind, and the attr value is the command.

        A key combination can consist of multiple keys in succession. Multiple
        commands for a single key combination can be defined by separating each
        command with a ;. Single and double quotes are honoured, as is escaping
        with a backslash, to allow the proper quoting of shell commands.

        Single keys such as `q` are just that: `{ q = "quit"; }` will bind the
        `q` key to the `quit` command.

        Modifier keys can be specified by prefixing them: `Ctrl+q`, `Meta+f`,
        `Shift+G`.  If multiple modifier keys are desired, they are specified in
        the order `Ctrl+Meta+Shift`.  When a key’s name is more than a single
        character, or a modifier is used it must be wrapped in `<` and `>`, for
        example: `<Ctrl+q>`.

        Multiple keys in succession can be specified by listing them in order:
        `{ gg = "goto 0"; }` will bind two presses of the `g` key to jump to the
        first image, and `{ "<Ctrl+a>p" = "exec echo hi"; }` will bind the key
        sequence of `Ctrl+a` followed by `p` to executing the shell command
        `echo hi`.

        Many keys, such as `<`, and `>` have special names. On X11, these can be
        easily found with the `xev(1)` command. For example, `!` is called
        `exclam`, `<` is called `less`, `>` is called `greater`.

        A complete list of keysyms can also be found on most systems with the
        `dumpkeys -l` command.
      '';
      default = {};
    };
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."imv/config".text = ''
      [options]
      background = ${cfg.background}
      fullscreen = ${lib.boolToString cfg.fullscreen}
      width = ${toString cfg.width}
      height = ${toString cfg.height}
      initial_pan = ${toString cfg.initialPan.x} ${toString cfg.initialPan.y}
      list_files_at_exit = ${lib.boolToString cfg.listFilesAtExit}
      loop_input = ${lib.boolToString cfg.loopInput}
      overlay = ${lib.boolToString cfg.overlay}
      overlay_text = ${cfg.overlayText}
      recursively = ${lib.boolToString cfg.recursively}
      scaling_mode = ${cfg.scalingMode}
      slideshow_duration = ${toString cfg.slideshowDuration}
      suppress_default_binds = ${lib.boolToString cfg.suppressDefaultBinds}
      title_text = ${cfg.titleText}
      upscaling_method = ${cfg.upscalingMethod}

      [aliases]
      ${lib.generators.toKeyValue {} cfg.aliases}

      [binds]
      ${lib.generators.toKeyValue {} cfg.binds}
    '';
  };
}
