{ config, lib, ... }:
let
  cfg = config.programs.newt;
in
{
  options.programs.newt.enable = lib.mkEnableOption "Newt";

  config = lib.mkIf cfg.enable {
    home.file.".newt.yml".text = ''
      auto-upgrade:
        enabled: false
    '';
  };
}
