{ config, ... }:

{
  home-manager.users.${config.primaryUserName}.home.file.".metatron/config".text = ''
    refresh.skipUpdateSshConfig = true
    configbrowser.omitBrowserTrustStoreImportConfirmation = true
  '';
}
