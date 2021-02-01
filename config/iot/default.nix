{ nixpkgs ? (import ../../sources.nix).nixpkgs }:
let
  pkgs = import nixpkgs { };

  templateOptions = config: config // {
    id = builtins.replaceStrings [ " " "'" ] [ "_" "" ] (pkgs.lib.toLower config.name);
    secrets = {
      wifi = builtins.extraBuiltins.getPasswordValue pkgs "Wifi/Centar";
      ap = builtins.extraBuiltins.getPasswordFieldValue pkgs "Infrastructure/IoT/${config.name}" "AP";
      api = builtins.extraBuiltins.getPasswordFieldValue pkgs "Infrastructure/IoT/${config.name}" "API";
      ota = builtins.extraBuiltins.getPasswordFieldValue pkgs "Infrastructure/IoT/${config.name}" "OTA";
    };
  };

  mkDevice = template: config:
    let
      options = templateOptions config;
    in
    {
      inherit (config) name;
      value = pkgs.writeText "${options.id}.yaml" (
        builtins.toJSON (template options)
      );
    };

  mkDevices = template: deviceNames:
    builtins.listToAttrs (map (mkDevice template) deviceNames);

  fans = mkDevices (pkgs.callPackage ./fan.nix { }) [
    { name = "Aiden's Room Fan"; }
    { name = "Guest Room Fan"; }
    { name = "Master Bedroom Fan"; }
    { name = "Office Fan"; }
  ];

  dimmers = mkDevices (pkgs.callPackage ./dimmer.nix { }) [
    { name = "Aiden's Room Lights"; }
    { name = "Bar Lights"; }
    { name = "Dining Room Lights"; }
    { name = "Entertainment Center Lights"; }
    { name = "Guest Room Lights"; }
    { name = "Hall Lights"; }
    { name = "Kitchen Lights"; }
    { name = "Living Room Lights"; }
    { name = "Master Bedroom Lights"; }
    { name = "Office Lights"; }
    { name = "Family Room Lights"; }
  ];

  plugs = mkDevices (pkgs.callPackage ./plug.nix { }) [
    { name = "Left Turtle Tank Lamp"; }
    { name = "Right Turtle Tank Lamp"; }
    { name = "Christmas Tree"; }
  ];

  dual_plugs = mkDevices (pkgs.callPackage ./dual_plug.nix { }) [
    { name = "Dual Plug 1"; plug1.name = "Tree Farm"; plug2.name = "Candy Cane Bannister"; }
    { name = "Dual Plug 2"; plug1.name = "Hole Border Lights"; plug2.name = "Penguin Projectors"; }
    { name = "Dual Plug 3"; plug1.name = "Gutter Lights"; plug2.name = "Moravian Stars"; }
  ];

  switches = mkDevices (pkgs.callPackage ./switch.nix { }) [
    { name = "Entry Light"; }
    { name = "Front Porch Light"; }
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
