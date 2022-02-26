{
  pkgs,
  lib,
  config,
  ...
}: let
  cfg = config.programs.swaylock;

  printValue = value:
    if builtins.isString value || builtins.isPath value
    then value
    else toString value;

  printConfigSetting = key: value:
    if builtins.isBool value
    then key
    else "${key}=${printValue value}";

  isValidConfigFileAttr = name: value:
    name
    != "enable"
    && (
      if builtins.isBool value
      then value
      else value != null
    );
in {
  options.programs.swaylock = {
    enable = lib.mkEnableOption "Swaylock";

    debug = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = "Enable debugging output.";
    };

    ignore-empty-password = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = ''
        When an empty password is provided by the user, do not validate it.
      '';
    };

    show-failed-attempts = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = ''
        Show the number of failed authentication attempts on the indicator.
      '';
    };

    daemonize = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = ''
        Detach from the controlling terminal after locking.

        Note: this is the default behavior of i3lock.
      '';
    };

    no-unlock-indicator = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = "Disable the unlock indicator.";
    };

    image = lib.mkOption {
      type = lib.types.nullOr (lib.types.oneOf [lib.types.str lib.types.path]);
      default = null;
      description = ''
        Display the given image, optionally only on the given output. Use -c to
        set a background color. If the path potentially contains a ':', prefix
        it with another
      '';
    };

    show-keyboard-layout = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = ''
        Force displaying the current xkb layout while typing, even if only one
        layout is configured.
      '';
    };

    hide-keyboard-layout = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = ''
        Force hiding the current xkb layout while typing, even if more than one
        layout is configured or the show-keyboard-layout option is set.
      '';
    };

    disable-caps-lock-text = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = "Disable the Caps Lock Text.";
    };

    indicator-caps-lock = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = "Show the current Caps Lock state also on the indicator.";
    };

    scaling = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.enum [
          "stretch"
          "fill"
          "fit"
          "center"
          "tile"
          "solid_color"
        ]
      );
      default = null;
      description = ''
        Scaling mode for images: stretch, fill, fit, center, or tile. Use the
        additional mode solid_color to display only the background color, even
        if a background image is specified.
      '';
    };

    color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Turn the screen into the given color. If -i is used, this sets the
        background of the image to the given color. Defaults to white (FFFFFF).
      '';
    };

    bs-hl-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Sets the color of backspace highlight segments.";
    };

    caps-lock-bs-hl-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of backspace highlight segments when Caps Lock is active.
      '';
    };

    caps-lock-key-hl-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the key press highlight segments when Caps Lock is
        active.
      '';
    };

    font = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Sets the font of the text inside the indicator.";
    };

    indicator-radius = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = ''
        Sets the radius of the indicator to radius pixels. The default value is
        50.
      '';
    };

    indicator-thickness = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = ''
        Sets the thickness of the indicator to thickness pixels. The default
        value is 10.
      '';
    };

    inside-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the inside of the indicator when typing or idle.
      '';
    };

    inside-clear-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the inside of the indicator when cleared.
      '';
    };

    inside-caps-lock-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the inside of the indicator when Caps Lock is active.
      '';
    };

    inside-ver-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the inside of the indicator when verifying.
      '';
    };

    inside-wrong-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the inside of the indicator when invalid.
      '';
    };

    key-hl-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Sets the color of key press highlight segments.";
    };

    layout-bg-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the background color of the box containing the layout text.
      '';
    };

    layout-border-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the border of the box containing the layout text.
      '';
    };

    layout-text-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Sets the color of the layout text.";
    };

    line-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the lines that separate the inside and outside of the
        indicator when typing or idle.
      '';
    };

    line-clear-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the lines that separate the inside and outside of the
        indicator when cleared.
      '';
    };

    line-caps-lock-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the line between the inside and ring when Caps Lock is
        active.
      '';
    };

    line-ver-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the lines that separate the inside and outside of the
        indicator when verifying.
      '';
    };

    line-wrong-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the lines that separate the inside and outside of the
        indicator when invalid.
      '';
    };

    line-uses-inside = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = ''
        Use the color of the inside of the indicator for the line separating the
        inside and outside of the indicator.
      '';
    };

    line-uses-ring = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = ''
        Use the outer ring's color for the line separating the inside and
        outside of the indicator.
      '';
    };

    ring-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the outside of the indicator when typing or idle.
      '';
    };

    ring-clear-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the outside of the indicator when cleared.
      '';
    };

    ring-caps-lock-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the ring of the indicator when Caps Lock is active.
      '';
    };

    ring-ver-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the outside of the indicator when verifying.
      '';
    };

    ring-wrong-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the outside of the indicator when invalid.
      '';
    };

    separator-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the lines that separate highlight segments.
      '';
    };

    text-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the text inside the indicator when typing or idle.
      '';
    };

    text-clear-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the text inside the indicator when cleared.
      '';
    };

    text-caps-lock-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Sets the color of the text when Caps Lock is active.";
    };

    text-ver-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the text inside the indicator when verifying.
      '';
    };

    text-wrong-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Sets the color of the text inside the indicator when invalid.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [pkgs.swaylock];
    xdg.configFile."swaylock/config".text =
      builtins.concatStringsSep "\n" (
        lib.mapAttrsToList printConfigSetting (
          lib.filterAttrs isValidConfigFileAttr cfg
        )
      );
  };
}
