{
  pkgs,
  colors,
  gmail-new-mail-counter,
  address,
  indicatorName,
  launcher,
}: let
  get-mail = pkgs.writeShellScript "get-mail" ''
    ${gmail-new-mail-counter}/bin/gmail_new_mail_counter \
      --format '{{#if (gt total 0)}}{"text":"✉ ${indicatorName} {{ unread }} / {{ total }}"{{#if (gt unread 0) }},"class":"unread"{{/if}}}{{/if}}' \
      --auth-format '{"text":"✉ ${indicatorName} "}' \
      ${address}
  '';
in {
  name = "custom/email-${builtins.replaceStrings ["@" "."] ["-" "-"] address}";

  config = {
    exec = get-mail;
    return-type = "json";
    interval = 20;
    on-click = "${launcher}/bin/browse https://mail.google.com?authuser=${address}";
  };

  style = {
    ".unread" = {
      font-weight = "bold";
      background-color = colors.selection;
      color = colors.background;
    };
    ".auth" = {
      background-color = colors.selection;
      color = colors.background;
    };
  };
}
