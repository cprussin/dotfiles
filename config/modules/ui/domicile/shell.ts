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
import {
  DEFAULT_KEYBINDINGS,
  DEFAULT_MODES,
  exec,
  moveToWorkspace,
  runManganese,
  workspace,
} from "@domicile-desktop/manganese";

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

export const Shell = runManganese({
  applications: @applications@,
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
