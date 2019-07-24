{ lib, config, ... }:

let
  nogit = toString ./.;
  link = withDot: file: {
    "${if withDot then "." else ""}${file}".source = "${nogit}/${file}";
  };
  linkFiles = withDot: lib.foldl (files: cur: files // (link withDot cur)) {};
in

{
  xdg = {
    configFile = linkFiles false [
      "chromium"
      "Slack"
      "StardewValley"
    ];

    dataFile = linkFiles false [ "Braid" ];
  };

  home.file = linkFiles true [
    "BitwigStudio"
    "factorio"
  ] // (let
    gpgPath = "${nogit}/secrets/gnupg";
    gpgFiles = builtins.readDir gpgPath;
    gpgFileLink = filename: _: lib.nameValuePair ".gnupg/${filename}" {
      source = "${gpgPath}/${filename}";
    };
  in
    lib.mapAttrs' gpgFileLink gpgFiles
  );
}
