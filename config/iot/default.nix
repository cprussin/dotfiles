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
  samsung_ac = mkDevice ./samsung_ac.nix;

  deviceConfigDir = name: devices:
    pkgs.linkFarmFromDrvs name (
      lib.mapAttrsToList (name: value: value name (mkId name)) devices
    );
in
  deviceConfigDir "iot-devices" {
    "Bedroom Thermostat" = samsung_ac {};
    "Dual Plug 1" = dual_plug {};
    "Dual Plug 2" = dual_plug {};
    "Dual Plug 3" = dual_plug {};
    "Dual Plug 4" = dual_plug {};
    "Plug 1" = plug {};
    "Plug 2" = plug {};
    "Plug 3" = plug {};
    "Plug 4" = plug {};
    "Plug 5" = plug {};
  }
