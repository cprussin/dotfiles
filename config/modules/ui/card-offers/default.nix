# card-offers — keep Amex/Chase card-linked offers added, with minimal manual work.
#
# The clicking is done by a small browser extension (./extension) that runs in
# your normal, logged-in Brave — so your password manager and device-trust are
# intact and login is painless. On the Amex/Chase offers pages it adds all
# available offers (automatically, or via a floating button). See ./README.md,
# including the one-time "load unpacked" install step.
#
# This module: installs the extension to a stable path, adds a launcher entry
# that opens the offers pages, and sets a weekly reminder.
{pkgs, ...}: let
  # Opens the offers pages in your default browser (via the launcher's `browse`).
  # Two backgrounded calls so both open as tabs rather than exec-replacing.
  open-offers = pkgs.writeShellScript "card-offers-open" ''
    ${pkgs.launcher}/bin/browse "https://global.americanexpress.com/offers/eligible" &
    ${pkgs.launcher}/bin/browse "https://secure.chase.com/web/auth/dashboard" &
    wait
  '';

  reminder = pkgs.writeShellScript "card-offers-reminder" ''
    ${pkgs.notify-send}/bin/notify-send \
      "Card offers" \
      "Time to add this week's Amex/Chase offers — open the 'card-offers' launcher; the extension adds them."
  '';
in {
  primary-user.home-manager = {
    # Stable path (survives rebuilds → stable extension id) to load unpacked from.
    home.file.".local/share/brave-extensions/card-offers".source = ./extension;

    # `card-offers` in the launcher opens both issuers' offers pages.
    programs.launcher.apps.card-offers = open-offers;

    # Weekly nudge so offers actually get added as they refresh.
    systemd.user = {
      services.card-offers-reminder = {
        Unit.Description = "Remind to add Amex/Chase card offers";
        Service = {
          Type = "oneshot";
          ExecStart = "${reminder}";
        };
      };
      timers.card-offers-reminder = {
        Unit.Description = "Weekly reminder to add card offers";
        Timer = {
          OnCalendar = "Mon 10:00";
          Persistent = true;
        };
        Install.WantedBy = ["timers.target"];
      };
    };
  };
}
