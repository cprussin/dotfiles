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
  primary-user.home-manager = {
    accounts.email = {
      maildirBasePath = "Mail";
      accounts = {
        "PrussinNet" = {
          address = "connor@prussin.net";
          primary = true;
          realName = "Connor Prussin";
          imap.host = "prussin.net";
          userName = "connor";
          passwordCommand = getPassword "Infrastructure/login/connor@prussin.net";
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
        "GMail" = {
          address = "cprussin@gmail.com";
          realName = "Connor Prussin";
          flavor = "gmail.com";
          passwordCommand = getAppPassword "Computer Services/Google";
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
      preExec = "${pkgs.emacs}/bin/emacsclient -e '(mu4e-update-index)'";
      postExec = "${pkgs.emacs}/bin/emacsclient -e '(mu4e-update-index)'";
    };

    systemd.user.services = {
      mbsync.Service.Environment = "\"PASSWORD_STORE_GPG_OPTS='--pinentry-mode cancel'\"";
      mbsync-manual = {
        Unit = {
          Description = "mbsync manual mailbox synchronization (unlock key store)";
          PartOf = [ "network-online.target" ];
        };
        Service = config.primary-user.home-manager.systemd.user.services.mbsync.Service // { Environment = ""; };
      };
    };
  };
}
