{ nixpkgs ? (import ../../sources.nix).nixpkgs }:
let
  pkgs = import nixpkgs { };

  passwords = pkgs.callPackage ../../lib/passwords.nix { };

  templateOptions = config: config // {
    id = builtins.replaceStrings [ " " "'" ] [ "_" "" ] (pkgs.lib.toLower config.name);
    secrets = {
      wifi = passwords.get-password "Wifi/Centar";
      ap = passwords.get-password-field "Infrastructure/IoT/${config.name}" "AP";
      api = passwords.get-password-field "Infrastructure/IoT/${config.name}" "API";
      ota = passwords.get-password-field "Infrastructure/IoT/${config.name}" "OTA";
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
    #{ name = "Family Room Lights"; }
    #{ name = "Fireplace Lights"; }
  ];

  plugs = mkDevices (pkgs.callPackage ./plug.nix { }) [
    { name = "Left Turtle Tank Lamp"; }
    { name = "Right Turtle Tank Lamp"; }
  ];

  switches = mkDevices (pkgs.callPackage ./switch.nix { }) [
    { name = "Entry Light"; }
    { name = "Front Porch Light"; }
    #{ name = "Back Porch Light"; }
    #{ name = "Trash Light"; }
  ];

  devices = fans // dimmers // plugs // switches;
in
devices // {
  fans = builtins.attrValues fans;
  dimmers = builtins.attrValues dimmers;
  plugs = builtins.attrValues plugs;
  switches = builtins.attrValues switches;
  all = builtins.attrValues devices;
}
