{ lib, config, ... }:

let
  nogit = toString ./.;
  link = withDot: file: {
    "${if withDot then "." else ""}${file}".source = "${nogit}/${file}";
  };
  linkFiles = withDot: lib.foldl (files: cur: files // (link withDot cur)) {};
in

{
  options.secrets = lib.mkOption {
    type = lib.types.str;
    default = "${nogit}/secrets";
    description = "A string containing the path to the secrets directory";
  };

  config = {
    xdg = {
      configFile = linkFiles false [
        "chromium"
        "keepassxc"
        "Slack"
        "StardewValley"
      ];

      dataFile = linkFiles false [
        "3909"
        "Aspyr"
        "Braid"
        "Steam"
        "SuperMeatBoy"
      ];
    };

    home.file = linkFiles true [
      "BitwigStudio"
      "factorio"
    ] // (let
      gpgPath = "${config.secrets}/gnupg";
      gpgFiles = builtins.readDir gpgPath;
      gpgFileLink = filename: _: lib.nameValuePair ".gnupg/${filename}" {
        source = "${gpgPath}/${filename}";
      };
    in
      lib.mapAttrs' gpgFileLink gpgFiles
    );
  };
}
