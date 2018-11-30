{ pkgs, config, ... }:

{
  home.packages = [ pkgs.mu ];

  accounts.email = {
    maildirBasePath = "Mail";
    accounts = {
      "PrussinNet" = {
        address = "connor@prussin.net";
        primary = true;
        realName = "Connor Prussin";
        imap.host = "prussin.net";
        userName = "connor";
        passwordCommand = "sed -n 2p ${config.secrets}/prussin.net";
        maildir.path = "PrussinNet";
        msmtp.enable = true;
        smtp = {
          port = 587;
          host = "prussin.net";
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          remove = "none";
        };
      };
      "Netflix" = {
        address = "cprussin@netflix.com";
        realName = "Connor Prussin";
        flavor = "gmail.com";
        passwordCommand = "sed -n 2p ${config.secrets}/netflix/google";
        maildir.path = "Netflix";
        msmtp.enable = true;
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          remove = "none";
          patterns = [
            "INBOX"
            "\\[Gmail\\]/Drafts"
            "\\[Gmail\\]/Spam"
          ];
        };
      };
      "GMail" = {
        address = "cprussin@gmail.com";
        realName = "Connor Prussin";
        flavor = "gmail.com";
        passwordCommand = "sed -n 2p ${config.secrets}/google";
        maildir.path = "GMail";
        msmtp.enable = true;
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          remove = "none";
          patterns = [
            "INBOX"
            "\\[Gmail\\]/All Mail"
            "\\[Gmail\\]/Drafts"
            "\\[Gmail\\]/Spam"
          ];
        };
      };
    };
  };
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  services.mbsync = {
    enable = true;
    preExec = "${config.programs.emacs.finalPackage}/bin/emacsclient -e '(mu4e-update-index)'";
    postExec = "${config.programs.emacs.finalPackage}/bin/emacsclient -e '(mu4e-update-index)'";
  };
}
