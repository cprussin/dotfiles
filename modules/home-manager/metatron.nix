{ config, lib, ... }:
let
  cfg = config.programs.metatron;
in
{
  options.programs.metatron = {
    enable = lib.mkEnableOption "Metatron";

    confirmBrowserTrustStoreImport = lib.mkOption {
      type = lib.types.bool;
      description = ''
        Confirm before importing Netflix Root CAs into browser truststore.
      '';
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    home.file.".metatron/config".text = ''
      refresh.skipUpdateSshConfig = true
      configbrowser.omitBrowserTrustStoreImportConfirmation = ${lib.boolToString (!cfg.confirmBrowserTrustStoreImport)}
    '';
  };
}
