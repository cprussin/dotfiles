{ lib, config, ... }:
let
  cfg = config.programs.fzf;

  mkColorOption = description: lib.mkOption {
    type = lib.types.str;
    description = "The color for ${description}";
  };

  colors = lib.concatStringsSep "," (
    lib.mapAttrsToList (k: v: "${k}:${v}") {
      fg = cfg.colors.fg;
      bg = cfg.colors.bg;
      hl = cfg.colors.hl;
      "fg+" = cfg.colors."fg+";
      "bg+" = cfg.colors."bg+";
      "hl+" = cfg.colors."hl+";
      info = cfg.colors.info;
      prompt = cfg.colors.prompt;
      pointer = cfg.colors.pointer;
      marker = cfg.colors.marker;
      spinner = cfg.colors.spinner;
      header = cfg.colors.header;
      gutter = cfg.colors.gutter;
    }
  );
in
{
  options.programs.fzf = {
    inline-info = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Display finder info inline with the query";
    };

    colors = lib.mkOption {
      description = "The color scheme to use.";
      default = null;
      type = lib.types.nullOr (
        lib.types.submodule {
          options = {
            fg = mkColorOption "Text";
            bg = mkColorOption "Background";
            hl = mkColorOption "Highlighted substrings";
            "fg+" = mkColorOption "Text (current line)";
            "bg+" = mkColorOption "Background (current line)";
            "hl+" = mkColorOption "Highlighted substrings (current line)";
            info = mkColorOption "Info";
            prompt = mkColorOption "Prompt";
            pointer = mkColorOption "Pointer to the current line";
            marker = mkColorOption "Multi-select marker";
            spinner = mkColorOption "Streaming input indicator";
            header = mkColorOption "Header";
            gutter = mkColorOption "Gutter on the left";
          };
        }
      );
    };
  };

  config = lib.mkIf cfg.enable {
    programs.fzf.defaultOptions = lib.flatten [
      (if cfg.colors == null then [ ] else [ "--color=${colors}" ])
      (if cfg.inline-info then [ "--inline-info" ] else [ ])
    ];
  };
}
