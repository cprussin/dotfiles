{ pkgs, lib, config, ... }:

let
  cfg = config.programs.waybar;

  styleBlockType = lib.types.attrsOf (
    lib.types.oneOf [
      lib.types.str
      (lib.types.attrsOf lib.types.str)
    ]
  );

  styleType = lib.types.attrsOf styleBlockType;

  moduleModule = lib.types.submodule {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        description = "The name of this module";
      };

      styleSelector = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "The root level style selecctor for this module";
      };

      config = lib.mkOption {
        type = lib.types.attrs;
        default = {};
        description = "The module config";
      };

      style = lib.mkOption {
        type = lib.types.nullOr styleType;
        default = null;
        description = "The module style";
      };
    };
  };

  allModules = cfg.modules.left ++ cfg.modules.center ++ cfg.modules.right;

  getModuleConfigs =
    lib.foldl (acc: module: acc // { ${module.name} = module.config; }) {};
  getName = map (builtins.getAttr "name");

  printCssBlockContents = lib.mapAttrsToList (
    attr: value:
      if builtins.isString value
      then "${attr}:${value};"
      else printCssBlock attr value
  );

  printCssBlock = selector: block:
    "${selector}{${lib.concatStrings (printCssBlockContents block)}}";

  printCssBlockSet = selectorPrefix: blockSet:
    builtins.concatStringsSep "\n" (
      lib.mapAttrsToList (
        selector: block:
          "${selectorPrefix}${printCssBlock selector block}"
      ) blockSet
    );

  defaultStyleSelector = module:
    "#" + builtins.replaceStrings [ "/" "#" ] [ "-" "." ] (
      lib.removePrefix "sway/" module.name
    );

  styleSelector = module:
    if module.styleSelector == null
    then defaultStyleSelector module
    else module.styleSelector;

  commonStyleSelector = builtins.concatStringsSep "," (
    map styleSelector allModules
  );
in

{
  options.programs.waybar = {
    enable = lib.mkEnableOption "Waybar";

    layer = lib.mkOption {
      type = lib.types.enum [ "top" " bottom" ];
      default = "bottom";
      description = ''
        Decide if the bar is displayed in front of the windows or behind them.
      '';
    };

    output = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.oneOf [
          (lib.types.listOf lib.types.str)
          lib.types.str
        ]
      );
      default = null;
      description = "Specifies on which screen this bar will be displayed.";
    };

    position = lib.mkOption {
      type = lib.types.enum [ "top" "bottom" "left" "right" ];
      default = "top";
      description = "Bar position, can be top, bottom, left, right.";
    };

    height = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = ''
        Height to be used by the bar if possible. Leave blank for a dynamic
        value.
      '';
    };

    width = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = ''
        Width to be used by the bar if possible. Leave blank for a dynamic
        value.
      '';
    };

    margin = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Margins value using the CSS format without units.";
    };

    margin-top = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Margin value without units.";
    };

    margin-left = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Margin value without units.";
    };

    margin-bottom = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Margin value without units.";
    };

    margin-right = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Margin value without units.";
    };

    name = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Optional name added as a CSS class, for styling multiple waybars.
      '';
    };

    modules = lib.mkOption {
      type = lib.types.submodule {
        options = {
          left = lib.mkOption {
            type = lib.types.listOf moduleModule;
            default = [];
            description = "Modules that will be displayed on the left.";
          };

          center = lib.mkOption {
            type = lib.types.listOf moduleModule;
            default = [];
            description = "Modules that will be displayed in the center.";
          };

          right = lib.mkOption {
            type = lib.types.listOf moduleModule;
            default = [];
            description = "Modules that will be displayed on the right.";
          };
        };
      };
    };

    styles = lib.mkOption {
      default = {};
      type = lib.types.submodule {
        options = {
          pre = lib.mkOption {
            type = lib.types.nullOr styleType;
            default = null;
            description = ''
              Extra style config to add to the beginning of styles.css
            '';
          };

          common = lib.mkOption {
            type = lib.types.nullOr styleBlockType;
            default = null;
            description = "Styles to apply to every module.";
          };

          frame = lib.mkOption {
            type = lib.types.nullOr styleBlockType;
            default = null;
            description = "Styles to apply to to the wrapping frame.";
          };

          post = lib.mkOption {
            type = lib.types.nullOr styleType;
            default = null;
            description = "Extra style config to add to the end of styles.css";
          };
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.waybar ];
    xdg.configFile = {
      "waybar/config".text = builtins.toJSON (
        lib.filterAttrs (_: v: v != null) (
          getModuleConfigs allModules // {
            layer = cfg.layer;
            output = cfg.output;
            position = cfg.position;
            height = cfg.height;
            width = cfg.width;
            margin = cfg.margin;
            name = cfg.name;
            margin-top = cfg.margin-top;
            margin-left = cfg.margin-left;
            margin-bottom = cfg.margin-bottom;
            margin-right = cfg.margin-right;
            modules-left = getName cfg.modules.left;
            modules-center = getName cfg.modules.center;
            modules-right = getName cfg.modules.right;
          }
        )
      );
      "waybar/style.css".text = lib.concatStringsSep "\n" (
        lib.flatten [
          (
            lib.optionalString (cfg.styles.pre != null) (
              printCssBlockSet "" cfg.styles.pre
            )
          )
          (
            lib.optionalString (cfg.styles.frame != null) (
              printCssBlock "window#waybar" cfg.styles.frame
            )
          )
          (
            lib.optionalString (cfg.styles.common != null) (
              printCssBlock commonStyleSelector cfg.styles.common
            )
          )
          (
            map (module: printCssBlockSet (styleSelector module) module.style) (
              lib.filter (module: module.style != null) allModules
            )
          )
          (
            lib.optionalString (cfg.styles.post != null) (
              printCssBlockSet "" cfg.styles.post
            )
          )
        ]
      );
    };
  };
}
