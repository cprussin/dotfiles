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
      # `xdg.dataFile` rather than `xdg.desktopEntries`, which would be the
      # obvious option: it installs through `home.packages`, at the normal
      # priority that every `lib.mkForce [...]` on that list in this repo
      # discards -- including the one in the module that imports this.  The
      # entry would silently not exist, and mimeApps below would then point
      # every web link at nothing at all.  This lands it in
      # ~/.local/share/applications, which the spec searches ahead of
      # XDG_DATA_DIRS, so it also doesn't lean on `environment.pathsToLink`.
      #
      # %u, not %U: `browse` keeps one target and drops the rest, so claiming
      # to accept a list would silently lose all but the last URL.
      dataFile."applications/browse.desktop".source = "${pkgs.makeDesktopItem {
        name = "browse";
        desktopName = "Browse";
        genericName = "Web Browser";
        exec = "${pkgs.launcher}/bin/browse %u";
        terminal = false;
        categories = ["Network" "WebBrowser"];
        mimeTypes = [
          "text/html"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
        ];
      }}/share/applications/browse.desktop";

      # This makes ~/.config/mimeapps.list a read-only store symlink, so
      # `xdg-mime default`, `xdg-settings set default-web-browser` and a
      # browser's own "make me the default" prompt all stop working.  Defaults
      # get set here and redeployed, not set at runtime.
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
