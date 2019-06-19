{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      setterminfo = super.callPackage ./setterminfo.nix {
        terminfo = pkgs.rxvt_unicode.terminfo;
      };
    })
  ];

  terminal = "${config.programs.urxvt.package}/bin/urxvtc";

  programs.urxvt = {
    enable = true;
    package = pkgs.rxvt_unicode-with-plugins;
    fonts = [ "xft:DejaVu Sans Mono:size=10" ];
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
      ExecStart = "${config.programs.urxvt.package}/bin/urxvtd";
    };
  };
}
