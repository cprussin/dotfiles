{ config, ... }:

{
  home.file.".metatron/config".text = ''
    refresh.skipUpdateSshConfig = true
    configbrowser.omitBrowserTrustStoreImportConfirmation = true
  '';
}
