{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.programs.launcher;
in {
  options.programs.launcher = {
    enable = lib.mkEnableOption "launcher";

    apps = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = {};
    };
  };

  config = lib.mkIf cfg.enable {
    home = {
      packages = [pkgs.launcher];

      sessionVariables = {
        EDITOR = "${pkgs.launcher}/bin/open";
        BROWSER = "${pkgs.launcher}/bin/browse";
      };

      file.".launcher-apps".source = pkgs.linkFarm "launcher-apps" (
        lib.mapAttrsToList (name: path: {inherit name path;}) cfg.apps
      );
    };
  };
}
