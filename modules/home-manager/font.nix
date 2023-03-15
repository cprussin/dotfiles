{
  lib,
  config,
  ...
}: let
  cfg = config.font;

  genDefault = fonts: name: ''
    <alias binding="same">
      <family>${name}</family>
      <prefer>
        ${lib.concatStringsSep "" (map (font: "<family>${font.face}</family>") fonts)}
      </prefer>
    </alias>
  '';

  fontModule = lib.types.submodule {
    options = {
      package = lib.mkOption {
        type = lib.types.package;
        description = "The package that provides the font";
      };

      face = lib.mkOption {
        type = lib.types.str;
        description = "The font face name.";
      };
    };
  };
in {
  options.font = lib.mkOption {
    default = null;
    type = lib.types.nullOr (lib.types.submodule {
      options = {
        sansSerif = lib.mkOption {
          type = lib.types.listOf fontModule;
        };
        serif = lib.mkOption {
          type = lib.types.listOf fontModule;
        };
        monospace = lib.mkOption {
          type = lib.types.listOf fontModule;
        };
        emoji = lib.mkOption {
          type = lib.types.listOf fontModule;
        };
      };
    });
  };

  config = lib.mkIf (cfg != null) {
    home.packages = map (font: font.package) (cfg.sansSerif ++ cfg.serif ++ cfg.monospace ++ cfg.emoji);

    xdg.configFile."fontconfig/conf.d/53-default-fonts.conf".text = ''
      <?xml version='1.0'?>
      <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
      <fontconfig>
        ${genDefault cfg.sansSerif "sans-serif"}
        ${genDefault cfg.serif "serif"}
        ${genDefault cfg.monospace "monospace"}
        ${genDefault cfg.emoji "emoji"}
      </fontconfig>
    '';
  };
}
