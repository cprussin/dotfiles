{ pkgs, config, ... }:

let
  pass = "${pkgs.pass}/bin/pass";
  head = "${pkgs.coreutils}/bin/head";
  grep = "${pkgs.gnugrep}/bin/grep";
  sed = "${pkgs.gnused}/bin/sed";
  getPassword = service: "${pass} show '${service}' | ${head} -n 1";
  getAppPassword = service: "${pass} show '${service}' | ${grep} \"App Password\" | ${sed} 's/.*: //'";
in

{
  accounts.email = {
    maildirBasePath = "Mail";
    accounts = {
      "PrussinNet" = {
        address = "connor@prussin.net";
        primary = true;
        realName = "Connor Prussin";
        imap.host = "prussin.net";
        userName = "connor";
        passwordCommand = getPassword "Infrastructure/prussin.net";
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
        passwordCommand = getAppPassword "Computer Services/Google (Netflix)";
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
        passwordCommand = getAppPassword "Computer Services/Google (Personal)";
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
