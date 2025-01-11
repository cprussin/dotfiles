{
  pkgs,
  lib,
}: let
  mkDevice = templateFile: config: name: id: let
    template = pkgs.callPackage templateFile {};
    options = config // {inherit name id;};
  in
    pkgs.writeText "${id}.yaml" (
      builtins.toJSON (
        pkgs.lib.recursiveUpdate (template options) (config.overrides or {})
      )
    );

  mkId = name: builtins.replaceStrings [" " "'"] ["-" ""] (lib.toLower name);
  fan = mkDevice ./fan.nix;
  dimmer = mkDevice ./dimmer.nix;
  dual_plug = mkDevice ./dual_plug.nix;
  switch = mkDevice ./switch.nix;
  plug = mkDevice ./plug.nix;

  deviceConfigDir = name: devices:
    pkgs.linkFarmFromDrvs name (
      lib.mapAttrsToList (name: value: value name (mkId name)) devices
    );
in
  deviceConfigDir "iot-devices" {
    "Aiden's Room Lights" = dimmer {};
    "Bar Lights" = dimmer {};
    "Bodhi's Room Lights" = dimmer {};
    "Dining Room Lights" = dimmer {};
    "Entertainment Center Lights" = dimmer {};
    "Family Room Lights" = dimmer {};
    "Guest Room Lights" = dimmer {};
    "Hall Lights" = dimmer {};
    "Kitchen Lights" = dimmer {};
    "Living Room Lights" = dimmer {};
    "Master Bedroom Lights" = dimmer {};
    "Garage Lights" = dimmer {};
    "Dual Plug 1" = dual_plug {};
    "Dual Plug 2" = dual_plug {};
    "Dual Plug 3" = dual_plug {};
    "Dual Plug 4" = dual_plug {};
    "Aiden's Room Fan" = fan {};
    "Bodhi's Room Fan" = fan {};
    "Guest Room Fan" = fan {};
    "Master Bedroom Fan" = fan {};
    "Plug 1" = plug {};
    "Plug 2" = plug {};
    "Plug 3" = plug {};
    "Plug 4" = plug {};
    "Plug 5" = plug {};
    "Entry Light" = switch {};
    "Front Porch Light" = switch {};
    "Trash Light" = switch {};
  }
