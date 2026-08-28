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

    # $BROWSER only reaches things that read it, which desktop apps mostly
    # don't -- they resolve a web link through the xdg handler for the scheme
    # and exec that .desktop's Exec line.  Without a default registered, that
    # picks whichever installed app happens to claim the scheme; the ChatGPT
    # app claims it for its in-app browser, so it was answering for every web
    # link on the machine.  Point both routes at the same script instead.
    xdg = {
      desktopEntries.browse = {
        name = "Browse";
        genericName = "Web Browser";
        exec = "${pkgs.launcher}/bin/browse %U";
        terminal = false;
        categories = ["Network" "WebBrowser"];
        mimeType = [
          "text/html"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
        ];
      };

      mimeApps = {
        enable = true;
        defaultApplications = {
          "text/html" = "browse.desktop";
          "x-scheme-handler/http" = "browse.desktop";
          "x-scheme-handler/https" = "browse.desktop";
        };
      };
    };
  };
}
