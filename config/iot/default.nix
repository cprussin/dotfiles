{ nixpkgs ? (import ../../sources.nix).nixpkgs }:
let
  pkgs = import nixpkgs { };

  templateOptions = config: config // {
    id = builtins.replaceStrings [ " " "'" ] [ "_" "" ] (pkgs.lib.toLower config.name);
  };

  mkDevice = template: config:
    let
      options = templateOptions config;
    in
    {
      inherit (config) name;
      value = pkgs.writeText "${options.id}.yaml" (
        builtins.toJSON (pkgs.lib.recursiveUpdate (template options) (config.overrides or { }))
      );
    };

  mkDevices = template: config:
    builtins.listToAttrs (map (mkDevice template) config);

  fans = mkDevices (pkgs.callPackage ./fan.nix { }) [
    { name = "Aiden's Room Fan"; }
    { name = "Bodhi's Room Fan"; }
    { name = "Guest Room Fan"; }
    { name = "Master Bedroom Fan"; }
  ];

  dimmers = mkDevices (pkgs.callPackage ./dimmer.nix { }) [
    { name = "Aiden's Room Lights"; }
    { name = "Bar Lights"; }
    { name = "Bodhi's Room Lights"; }
    { name = "Dining Room Lights"; }
    { name = "Entertainment Center Lights"; }
    { name = "Family Room Lights"; }
    { name = "Guest Room Lights"; }
    { name = "Hall Lights"; }
    { name = "Kitchen Lights"; }
    { name = "Living Room Lights"; }
    { name = "Master Bedroom Lights"; }
    { name = "Spare Dimmer 1"; }
  ];

  plugs = mkDevices (pkgs.callPackage ./plug.nix { }) [
    { name = "Left Turtle Tank Lamp"; }
    { name = "Right Turtle Tank Lamp"; }
    { name = "Plug 1"; }
    { name = "Plug 2"; }
    { name = "Plug 3"; }
  ];

  dual_plugs = mkDevices (pkgs.callPackage ./dual_plug.nix { }) [
    { name = "Dual Plug 1"; }
    { name = "Dual Plug 2"; }
    { name = "Dual Plug 3"; }
    { name = "Dual Plug 4"; }
  ];

  switches = mkDevices (pkgs.callPackage ./switch.nix { }) [
    { name = "Entry Light"; }
    { name = "Front Porch Light"; }
    { name = "Spare Switch 1"; }
  ];

  devices = fans // dimmers // plugs // dual_plugs // switches;
in
devices // {
  fans = builtins.attrValues fans;
  dimmers = builtins.attrValues dimmers;
  plugs = builtins.attrValues plugs;
  dual_plugs = builtins.attrValues plugs;
  switches = builtins.attrValues switches;
  all = builtins.attrValues devices;
}
