{ config, ... }:

{
  home.file.".metatron/config".text = ''
    refresh.skipUpdateSshConfig = true
    refresh.sshKeyPath = ${config.sshKeys}/metatron
    configbrowser.omitBrowserTrustStoreImportConfirmation = true
  '';
}
