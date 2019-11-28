{ pkgs, lib, config, ... }:

let
  cfg = config.programs.mako;

  globalOptions = {
    max-visible = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = ''
        Set maximum number of visible notifications to n. Older notifications
        will be hidden. If -1, all notifications are visible.
      '';
    };

    sort = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.enum [ "+time" "-time" "+priority" "-priority" ]
      );
      default = null;
      description = ''
        Sorts incoming notifications by time and/or priority in ascending(+) or
        descending(-) order.
      '';
    };

    output = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Show notifications on the specified output. If empty, notifications will
        appear on the focused output.

        Requires the compositor to support the Wayland protocol
        xdg-output-unstable-v1 version 2.
      '';
    };

    layer = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.enum [ "background" "bottom" "top" "overlay" ]
      );
      default = null;
      description = ''
        Arrange mako at the specified layer, relative to normal windows.
        Supported values are background, bottom, top, and overlay. Using overlay
        will cause notifications to be displayed above fullscreen windows,
        though this may also occur at top depending on your compositor.
      '';
    };

    anchor = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.enum [
          "top-right"
          "top-center"
          "top-left"
          "bottom-right"
          "bottom-center"
          "bottom-left"
          "center"
        ]
      );
      default = null;
      description = ''
        Show notifications at the specified position on the output. Supported
        values are top-right, top-center, top-left, bottom-right, bottom-center,
        bottom-left, and center.
      '';
    };

    font = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Set font to font, in Pango format.";
    };

    background-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Set background color to color.";
    };

    text-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Set text color to color.";
    };

    width = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Set width of notification popups.";
    };

    height = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = ''
        Set maximium height of notification popups. Notifications whose text
        takes up less space are shrunk to fit.
      '';
    };

    margin = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.oneOf [ lib.types.int (lib.types.listOf lib.types.int) ]
      );
      default = null;
      description = ''
        Set margin of each edge to the size specified by directional.
      '';
    };

    padding = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.oneOf [ lib.types.int (lib.types.listOf lib.types.int) ]
      );
      default = null;
      description = ''
        Set padding on each side to the size specified by directional.
      '';
    };

    border-size = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Set popup border size to px pixels.";
    };

    border-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Set popup border color to color.";
    };

    border-radius = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Set popup corner radius to px pixels.";
    };

    progress-color = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Set popup progress indicator color to color. To draw the progress
        indicator on top of the background color, use the over attribute. To
        replace the background color, use the source attribute (this can be
        useful when the notification is semi-transparent).
      '';
    };

    icons = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = "Show icons in notifications.";
    };

    max-icon-size = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Set maximum icon size to px pixels.";
    };

    icon-path = lib.mkOption {
      type = lib.types.nullOr (lib.types.listOf (lib.types.path));
      default = null;
      description = ''
        Paths to search for icons when a notification specifies a name instead
        of a full path. This approximates the search algorithm used by the XDG
        Icon Theme Specification, but does not support any of the theme
        metadata. Therefore, if you want to search parent themes, you'll need to
        add them to the path manually.

        /usr/share/icons/hicolor and /usr/share/pixmaps are always searched.
      '';
    };

    markup = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = ''
        Enable Pango markup. If enabled, Pango markup will be interpreted in
        your format specifier and in the body of notifications.
      '';
    };

    actions = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = ''
        Applications may request an action to be associated with activating a
        notification. Disabling this will cause mako to ignore these requests.
      '';
    };

    default-timeout = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = ''
        Set the default timeout to timeout in milliseconds. To disable the
        timeout, set it to zero.
      '';
    };

    ignore-timeout = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = ''
        If set, mako will ignore the expire timeout sent by notifications and
        use the one provided by default-timeout instead.
      '';
    };

    group-by = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.listOf (
          lib.types.enum [
            "app-name"
            "app-icon"
            "summary"
            "urgency"
            "category"
            "desktop-entry"
            "actionable"
            "expiring"
          ]
        )
      );
      default = null;
      description = ''
        A list of criteria fields that will be compared to other visible
        notifications to determine if this one should form a group with
        them. All listed criteria must be exactly equal for two notifications to
        group.
      '';
    };

    format = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Set notification format string to format.";
    };
  };

  criteriaModule = {
    options = {
      app-name = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      app-icon = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      summary = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          An exact match on the summary of the notification.
        '';
      };

      urgency = lib.mkOption {
        type = lib.types.nullOr (lib.types.enum [ "low" "normal" "high" ]);
        default = null;
      };

      category = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      desktop-entry = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      actionable = lib.mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
      };

      expiring = lib.mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
      };

      grouped = lib.mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
        description = ''
          Whether the notification is grouped with any others (its
          group-index is not -1).
        '';
      };

      group-index = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = ''
          The notification's index within its group, or -1 if it is not
          grouped.
        '';
      };

      hidden = lib.mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
        description = ''
          hidden is special, it defines the style for the placeholder
          shown when the number of notifications or groups exceeds
          max-visible.
        '';
      };
    };
  };

  groupModule = {
    options = globalOptions // {
      criteria = lib.mkOption {
        type = lib.types.submodule criteriaModule;
      };

      invisible = lib.mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
        description = ''
          Whether this notification should be invisible even if it is above
          the max-visible cutoff. This is used primarily for hiding members of
          groups.  If you want to make more than the first group member
          visible, turn this option off within a group-index criteria.
        '';
      };
    };
  };

  printValue = name: value:
    if (name == "margin" || name == "padding") && builtins.isList value
    then builtins.concatStringsSep "," (map toString value)
    else
      if name == "group-by"
      then
        if builtins.length value == 0
        then "none"
        else builtins.concatStringsSep "," value
      else
        if name == "icon-path"
        then builtins.concatStringsSep ":" value
        else
          if builtins.isString value
          then value
          else toString value;

  isValidConfigSectionAttr = name: value:
    value != null && builtins.all (attr: attr != name) [
      "enable"
      "groups"
      "criteria"
      "_module"
    ];

  configSection = group: builtins.concatStringsSep "\n" (
    lib.mapAttrsToList (name: value: "${name}=${printValue name value}") (
      lib.filterAttrs isValidConfigSectionAttr group
    )
  );

  isValidCriteria = name: val:
    name != "_module" && val != null;

  groupHeader = group:
    builtins.concatStringsSep " " (
      lib.mapAttrsToList (name: value: "${name}=${toString value}") (
        lib.filterAttrs isValidCriteria group.criteria
      )
    );

  groupConfig = group:
    builtins.concatStringsSep "\n" [
      "[${groupHeader group}]"
      (configSection group)
    ];
in

{
  options.programs.mako = globalOptions // {
    enable = lib.mkEnableOption "Mako";

    groups = lib.mkOption {
      default = [];
      type = lib.types.listOf (lib.types.submodule groupModule);
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.mako ];
    xdg.configFile."mako/config".text =
      builtins.concatStringsSep "\n\n" (
        [ (configSection cfg) ] ++ (map groupConfig cfg.groups)
      );
  };
}
