{ config, ... }:

{
  home-manager.users.${config.primary-user.name}.home.file.".metatron/config".text = ''
    refresh.skipUpdateSshConfig = true
    configbrowser.omitBrowserTrustStoreImportConfirmation = true
  '';
}
