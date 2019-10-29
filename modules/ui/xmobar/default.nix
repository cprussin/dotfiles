{ pkgs, config, ... }:

let
  email = pkgs.callPackage ./email.nix { inherit config; };
  vpn = pkgs.callPackage ./vpn.nix { inherit config; };
  volume = pkgs.callPackage ./volume.nix { inherit config; };
  bluetooth = pkgs.callPackage ./bluetooth.nix { inherit config; };
  battery = pkgs.callPackage ./battery.nix { inherit config; };
  network = pkgs.callPackage ./network.nix { inherit config; };
  date = pkgs.callPackage ./date.nix {};
  mounts = pkgs.callPackage ./mounts.nix {};
  fontLib = pkgs.callPackage ../../../lib/fonts.nix {};
in

{
  home-manager.users.${config.primaryUserName}.home.file.".xmobarrc".text = ''
    Config
      { font = "${fontLib.xftFont config.fontTheme.primaryFont}"
      , additionalFonts =
        [ "${fontLib.xftFont config.fontTheme.primaryFont}:style=Bold"
        , "xft:Font Awesome 5 Free:size=9:style=Solid"
        , "xft:Font Awesome 5 Brands:size=9"
        ]
      , position = BottomSize C 100 35
      , border = TopB
      , borderColor = "${config.colorTheme.selection}"
      , borderWidth = 3
      , textOffset = 25
      , textOffsets = [ 25, 25, 25 ]
      , bgColor = "${config.colorTheme.background}"
      , fgColor = "${config.colorTheme.bright}"
      , overrideRedirect = False
      , commands =
        [ Run UnsafeStdinReader
        , Run Com "${email}" [] "email" 10
        , Run Com "${vpn}" [] "vpn" 10
        , Run Com "${volume}" [] "volume" 1
        , Run Com "${bluetooth}" [] "bluetooth" 10
        , Run Com "${network}" [] "network" 10
        , Run Com "${battery}" [] "battery" 10
        , Run Com "${date}" [] "date" 10
        , Run Com "${mounts}" [] "mounts" 10
        ]
      , template = " %UnsafeStdinReader%}{%email%%vpn%%mounts%%bluetooth%%network%%volume%%battery%%date%        "
      }
  '';
}
