// This desk's shell: manganese, with its keys on Meta.
//
// `Meta+Return` opens the terminal, which manganese leaves for the config to
// bind, by the path ./default.nix fills in.  The launcher's applications and
// bookmarks are ./default.nix's `applications`, filled in as JSON.
//
// manganese's workspaces are on the digits.  On dvp the digits are shifted, so
// the workspaces go on the top row's own symbols instead -- `parenleft` is the
// first workspace.  domicile finds the key each keysym is on in the config's
// `input.keyboard`, so these follow the layout.
//
// The notification drawer lists each Gmail account's inbox, then Google
// Messages.  Gmail is read with the Gmail API through one Google Cloud OAuth
// client (Desktop app type).  Google does not treat a desktop client's secret
// as secret.
//
// manganese's `options` is re-exported so Settings' Shell page can edit them.
import { runManganese } from "@domicile-desktop/manganese";
import { gmailInbox } from "@domicile-desktop/manganese-inbox-gmail";
import { messagesInbox } from "@domicile-desktop/manganese-inbox-google-messages";
import {
  DEFAULT_KEYBINDINGS,
  exec,
  moveToWorkspace,
  workspace,
} from "@domicile-desktop/manganese-window-management/keyboard/commands";

/** Workspaces 1 to 10, in the order dvp's top row has them. */
const WORKSPACE_KEYS = [
  "parenleft",
  "parenright",
  "braceright",
  "plus",
  "braceleft",
  "bracketright",
  "bracketleft",
  "exclam",
  "equal",
  "asterisk",
];

/** A default that puts a workspace on a digit, which this desk does not. */
const onADigit = (chord: string): boolean => /^Meta\+(Shift\+)?\d$/.test(chord);

export { options } from "@domicile-desktop/manganese";

const GMAIL_CLIENT = {
  id: "218508107688-o7njpis2eip1akvq6vpo86tq3np4tkhm.apps.googleusercontent.com",
  secret: "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",
};

export const Shell = runManganese({
  applications: @applications@,
  inboxes: [
    gmailInbox({
      client: GMAIL_CLIENT,
      email: "connor@prussin.net",
      launcherName: "Email - PrussinNet",
      nickname: "PrussinNet",
    }),
    gmailInbox({
      client: GMAIL_CLIENT,
      email: "connor@meatproxylabs.com",
      launcherName: "Email - Meat Proxy Labs",
      nickname: "Meat Proxy",
    }),
    messagesInbox({ launcherName: "SMS" }),
  ],
  keybindings: {
    keybindings: {
      ...Object.fromEntries(
        Object.entries(DEFAULT_KEYBINDINGS).filter(
          ([chord]) => !onADigit(chord),
        ),
      ),
      "Meta+Return": exec("@terminal@"),
      ...Object.fromEntries(
        WORKSPACE_KEYS.flatMap((key, at) => [
          [`Meta+${key}`, workspace(String(at + 1))],
          [`Meta+Shift+${key}`, moveToWorkspace(String(at + 1))],
        ]),
      ),
    },
  },
});
