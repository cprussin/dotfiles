// This desk's shell: manganese, with sway's keys on Meta as ui/sway binds them.
//
// `Meta+Return` opens the terminal, which manganese leaves for the config to
// bind, by the path ./default.nix fills in.  The launcher's applications and
// bookmarks are ./default.nix's `applications`, filled in as JSON.
//
// manganese's defaults already are sway's, but for one thing: its workspaces
// are on the digits.  On dvp the digits are shifted, so the workspaces go on
// the top row's own symbols instead -- `parenleft` is the first workspace, as
// it is under sway.  domicile finds the key each keysym is on in the config's
// `input.keyboard`, so these follow the layout.
//
// Each Gmail account's inbox leads the bar's right column.
//
// manganese's `options` is re-exported so Settings' Shell page can edit them.
import { DEFAULT_BAR, runManganese } from "@domicile-desktop/manganese";
import { Gmail } from "@domicile-desktop/manganese-bar-module-gmail";
import {
  DEFAULT_KEYBINDINGS,
  DEFAULT_MODES,
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

export const Shell = runManganese({
  applications: @applications@,
  bar: {
    ...DEFAULT_BAR,
    right: [
      <Gmail
        email="connor@prussin.net"
        key="prussin-net"
        nickname="PrussinNet"
      />,
      <Gmail
        email="connor@meatproxylabs.com"
        key="meat-proxy"
        nickname="Meat Proxy"
      />,
      ...DEFAULT_BAR.right,
    ],
  },
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
    modes: DEFAULT_MODES,
  },
});
