{ pkgs, config, lib, ... }:

let
  urxvt = pkgs.rxvt_unicode-with-plugins;
in

{
  nixpkgs.overlays = [
    (
      _: super: {
        setterminfo = super.callPackage ./setterminfo.nix {
          terminfo = pkgs.rxvt_unicode.terminfo;
        };
      }
    )
  ];

  terminal = "${urxvt}/bin/urxvtc";

  home-manager.users.${config.primaryUserName} = {
    home.packages = lib.mkForce [ pkgs.setterminfo ];

    programs.urxvt = {
      enable = true;
      package = urxvt;
      scroll.bar.enable = false;
      keybindings = {
        "M-u" = "perl:url-select:select_next";
        "C-plus" = "font-size:increase";
        "C-minus" = "font-size:decrease";
      };
      extraConfig = {
        "perl-ext-common" = "default,clipboard,url-select,keyboard-select";
        "url-select.launcher" = "browse";
        "url-select.underline" = true;
      };
    };

    systemd.user.services.urxvtd = {
      Unit = {
        Description = "rxvt-unicode daemon";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = "${urxvt}/bin/urxvtd";
      };
    };
  };
}
