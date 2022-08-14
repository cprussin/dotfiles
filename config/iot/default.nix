{nixpkgs ? (import ../../sources.nix).nixpkgs}: let
  pkgs = import nixpkgs {};

  mkDevice = templateFile: name: config: let
    id = builtins.replaceStrings [" " "'"] ["-" ""] (
      pkgs.lib.toLower name
    );
    template = pkgs.callPackage templateFile {};
    options = config // {inherit name id;};
  in {
    inherit name;
    value = pkgs.writeText "${id}.yaml" (
      builtins.toJSON (
        pkgs.lib.recursiveUpdate (template options) (config.overrides or {})
      )
    );
  };

  fan = mkDevice ./fan.nix;
  dimmer = mkDevice ./dimmer.nix;
  dual_plug = mkDevice ./dual_plug.nix;
  switch = mkDevice ./switch.nix;
  plug = mkDevice ./plug.nix;

  devices = builtins.listToAttrs;
in
  devices [
    (dimmer "Aiden's Room Lights" {})
    (dimmer "Bar Lights" {})
    (dimmer "Bodhi's Room Lights" {})
    (dimmer "Dining Room Lights" {})
    (dimmer "Entertainment Center Lights" {})
    (dimmer "Family Room Lights" {})
    (dimmer "Guest Room Lights" {})
    (dimmer "Hall Lights" {})
    (dimmer "Kitchen Lights" {})
    (dimmer "Living Room Lights" {})
    (dimmer "Master Bedroom Lights" {})
    (dimmer "Spare Dimmer 1" {})
    (dual_plug "Dual Plug 1" {})
    (dual_plug "Dual Plug 2" {})
    (dual_plug "Dual Plug 3" {})
    (dual_plug "Dual Plug 4" {})
    (fan "Aiden's Room Fan" {})
    (fan "Bodhi's Room Fan" {})
    (fan "Guest Room Fan" {})
    (fan "Master Bedroom Fan" {})
    (plug "Left Turtle Tank Lamp" {})
    (plug "Plug 1" {})
    (plug "Plug 2" {})
    (plug "Plug 3" {})
    (plug "Right Turtle Tank Lamp" {})
    (switch "Entry Light" {})
    (switch "Front Porch Light" {})
    (switch "Spare Switch 1" {})
  ]
