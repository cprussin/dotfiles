# The same desk as kanshi, written for domicile.
#
# config/modules/ui/kanshi/default.nix is this desk under sway: same
# arithmetic, same monitor records, a profile chosen by what is plugged in and
# re-matched on every hotplug.  domicile differs in two ways that are not
# cosmetic -- it matches on a slightly different string, and it cannot pin a
# mode -- and lacks sway's workspace assignment, which it has no concept to
# assign.
#
# TWO WAYS THE NAMES DIFFER FROM THE KANSHI STRINGS, and both bite silently: a
# name that matches nothing is not an error to domicile, so the desk comes up
# unplaced and looks like this file was never read.
#
#   The make is the three-letter PNP id, not the vendor name.  "DEL", not
#   "Dell Inc.".
#
#   A missing serial is left out, not spelled "Unknown" the way sway spells it.
#
# So every name in `derived` below was worked out from the kanshi string next
# door through hwdata's pnp.ids, not read off a running desktop.
#
# EVERY POSITION ASSUMES A MODE NOTHING PINS.  kanshi sets one per output; a
# domicile profile has no mode field, so the mode arrives with the monitor and
# the compositor divides it by the scale.  Fine while the driver lights every
# connector at its native mode, which is the mode kanshi pins anyway -- and
# nothing here could correct a monitor that negotiated something else.
#
# THE TURN AND THE SCALE REACH THE GLASS, which they did not until recently:
# `rotate-270` was advertised and laid out but not drawn, and a 1.2 panel drew
# its desktop in the corner of a black screen.  Both need an engine carrying
# the fix -- `engine-release.nix` in domicile is where that pin lives -- and
# on an older pin they read as the old behaviour rather than as an error.
# Which way round the quarter turns are is the one thing only these monitors
# can settle; if they come up upside down the fix is in domicile's
# `cover-the-window.ts`, not here.
#
# THE FILE IS `programs.domicile`'S TO WRITE, not this one's.  domicile ships
# a home-manager module with an option per field, and this is that module's
# `settings` -- so where the file goes, and that it goes where `domicile` with
# no `--config` looks, are no longer facts this repository has to know.  What
# is left here is the desk: which monitors, where, and which way up.
#
# The two sides still reach that path differently -- home-manager from
# `xdg.configHome` at build time, domicile from `XDG_CONFIG_HOME` at start --
# so exporting one into the session without moving the other writes this where
# domicile does not look.  It is re-read on change, so a rebuild reaches a
# running desk, and domicile prints which file it chose before it starts
# anything.
#
# `enable` IS BOTH HALVES: it writes the file AND installs the package, which
# this module did not do before.  So `domicile` is on PATH on the one machine
# that imports this -- lyra -- and turning it off writes nothing at all.
# `mkDefault`, so a host can say so without `mkForce`: `enable` is a plain
# bool, and a bare `true` here would make a host's `false` a conflict rather
# than an override.  (Two bare `true`s would merge quietly -- it is the
# disagreement that is the error, not the second definition.)
#
# No shell is named, so `domicile` still takes one as an argument.  That is
# not the same as a login session: a session is a unit and a greeter, which is
# a NixOS-level decision about how this machine boots, and neither this file
# nor `programs.domicile` is where it would go.
{
  config,
  lib,
  ...
}: let
  cfg = config.ui.domicile;

  # The panels, in physical pixels at the density each is readable at.
  # `logical` is what the desktop is laid out in and what every position below
  # is measured in -- kept as floats, because the positions are sums of these
  # and rounding each one first would drift a pixel per monitor across the
  # desk.
  panel = scale: width: height: {
    inherit scale;
    logical = {
      width = width / scale;
      height = height / scale;
    };
  };

  laptopPanel = panel 1.5 2880 1920;
  portablePanel = panel 1.4 3840 2160;
  curved = panel 1.0 3440 1440;

  # The three on the desk are the same monitor three times over, and only
  # their ids differ.
  external = panel 1.2 3840 2160;
  left = external;
  center = external;
  right = external;

  # What a monitor measures once stood on its side: the one thing a transform
  # changes about the arithmetic.
  widthOnItsSide = monitor: monitor.logical.height;
  heightOnItsSide = monitor: monitor.logical.width;

  at = position: monitor: {
    inherit (monitor) scale;
    position = [(outward position.x) (outward position.y)];
  };

  # The turn the CONTENT takes to come out upright, which is the same `270`
  # the kanshi file writes for these three.  Drawn, not just described -- see
  # the header.
  sideways = position: monitor:
    (at position monitor) // {transform = "rotate-270";};

  off = _: {enabled = false;};

  # Whole pixels, rounded outward like the kanshi module: a monitor a pixel
  # further out leaves a gap the page spans and nobody sees, where one a pixel
  # short overlaps its neighbour -- and domicile has no mirroring, so an
  # overlap is two screens claiming one patch of desktop.
  outward = builtins.ceil;

  # Every monitor this desk has, so that a key which is not one of them is
  # caught rather than read as a monitor nobody has plugged in.
  monitors = {
    inherit laptopPanel portablePanel curved left center right;
  };

  # What each monitor is called, worked out from the kanshi string next door.
  #
  # NOT the option's `default`, which is the whole reason this attrset exists:
  # an `attrsOf` that is set REPLACES its default rather than merging, so a
  # machine correcting one name would silently lose the other five.
  derived = {
    laptopPanel = "BOE NE135A1M-NY1";
    left = "DEL DELL U3219Q 2ZLS413";
    center = "DEL DELL U3219Q G3MS413";
    right = "DEL DELL U3219Q H8KF413";
    curved = "GSM LG ULTRAWIDE 0x0001E368";
    # kanshi says "Audio Processing Technology  Ltd Monitor demoset-1": the
    # double space is pnp.ids' own, which is what says the model is "Monitor".
    portablePanel = "APT Monitor demoset-1";
  };

  # The option sway reads, not the NixOS one -- see where it is written out.
  keyboard = config.primary-user.home-manager.keymap;

  # What the machine said, else what was derived, else nothing.  `or` catches
  # a MISSING key rather than a null one, which is the point: setting a
  # monitor to `null` stops at the first rung instead of falling through to
  # `derived`, and that is how a wrong derived name is taken back out.
  named = key: cfg.displays.${key} or derived.${key} or null;

  # One profile, or nothing when a monitor it names has no id yet.  Dropped
  # whole, because one quietly missing a monitor would match a smaller desk
  # and put the whole arrangement up on it.
  profile = name: displays: let
    entries =
      lib.mapAttrsToList (key: placement: {
        display = named key;
        inherit placement;
      })
      displays;
  in
    lib.optional (lib.all (entry: entry.display != null) entries) {
      inherit name;
      displays = map (entry: entry.placement // {inherit (entry) display;}) entries;
    };
in {
  options.ui.domicile.displays = lib.mkOption {
    # The panel's own name, or the output's `drm-<id>` for a monitor that
    # states no make, model or serial.  Shaped rather than any string because
    # a name matching nothing is not an error to domicile -- it just leaves
    # the monitors where the engine put them.
    #
    # A panel name is not always three parts: the compositor joins whatever
    # the EDID has, so `DEL DELL U3219Q` and a bare `DEL` are both names it
    # advertises.  Hence three capitals, or two or more words.  That catches
    # the single-word mistakes -- a bare id with no `drm-`, sway's `DP-1` --
    # and not a kanshi string, which is words separated by spaces too.  The
    # id is signed because `DrmScreen` hands out `kDefaultDisplayId` for a
    # machine with nothing plugged in, so the sign is not assumed away.
    type = lib.types.attrsOf (lib.types.nullOr (lib.types.strMatching "(drm--?[0-9]+)|([A-Z]{3})|([^[:space:]]+( [^[:space:]]+)+)"));

    default = {};
    example = {
      # A derived name corrected, and a derived name taken back out: the two
      # things anybody writes here.
      left = "DEL DELL U3219Q 4XYZ123";
      center = "drm-92";
      portablePanel = null;
    };
    description = ''
      What domicile knows each monitor of this desk as.  One of
      ${lib.concatStringsSep ", " (lib.attrNames monitors)}.

      Either the panel's own name -- the make, model and serial its EDID
      states, which is what kanshi matches on give or take the vendor name --
      or the output's, which is `drm-<id>` for an id ozone derives from the
      same EDID.  Both survive a hotplug.

      A monitor left out keeps the name this file already derived for it, so
      nothing needs writing here unless one of those turns out wrong.  Set one
      to null to take a derived name back out: a monitor with no name is one
      nobody has established, and the profiles naming it are not written out
      at all, so an incomplete answer costs arrangements rather than producing
      wrong ones.
    '';
  };

  config = {
    # The module that writes the file, taken from the flake whose compositor
    # reads it: the schema is kept in step over there now rather than by hand
    # over here.
    primary-user.home-manager.imports = [config.flake-inputs.domicile.homeManagerModules.default];

    assertions = let
      unknown = lib.subtractLists (lib.attrNames monitors) (lib.attrNames cfg.displays);
      # The RESOLVED names, not `cfg.displays`: the mistake is one line
      # corrected by hand colliding with a name nobody wrote down because it
      # was already right.  Unset monitors dropped -- two nulls are not two
      # monitors sharing a name.
      ids = lib.filter (id: id != null) (map named (lib.attrNames monitors));
      repeated = lib.unique (lib.filter (id: lib.count (other: other == id) ids > 1) ids);
      # One line: `''` would keep the source's own line breaks.
      sentence = lib.concatStringsSep " ";
      list = lib.concatStringsSep ", ";
      places = "It places ${list (lib.attrNames monitors)}.";
      # Which monitors a name resolved to.  Named, because grepping for it
      # finds at most one -- the other was derived and never written down.
      sharing = id: lib.attrNames (lib.filterAttrs (key: _: named key == id) monitors);
      # "left and center".  Not `list`: mid-sentence a bare comma reads as
      # the end of a clause rather than as another monitor.
      andList = keys:
        if lib.length keys < 2
        then list keys
        else "${list (lib.init keys)} and ${lib.last keys}";
      # "both" only when there are two; one pasted serial can give you three.
      shared = keys:
        if lib.length keys == 2
        then "both ${andList keys}"
        else "all of ${andList keys}";
    in [
      # A key that is not a monitor is a silent typo: it names no profile, so
      # the ones it was meant for are never written and the desk comes up
      # unplaced with nothing to say why.
      {
        assertion = unknown == [];
        message = sentence [
          "ui.domicile.displays names ${list unknown},"
          (
            if lib.length unknown == 1
            then "which is not a monitor"
            else "which are not monitors"
          )
          "this desk has."
          places
        ];
      }
      # Easy to write -- left, center and right are the same panel three times
      # -- and expensive: domicile refuses a profile placing one display
      # twice, and that refuses the whole file.
      {
        assertion = repeated == [];
        message = sentence (
          map (id: "This desk knows ${id} as ${shared (sharing id)}.") repeated
          ++ [
            "Those are different panels, and domicile refuses the whole config"
            "file over a profile that places one display twice."
            "A name not written in ui.domicile.displays is one this file"
            "derived."
          ]
        );
      }
    ];

    primary-user.home-manager.programs.domicile = {
      enable = lib.mkDefault true;
      settings = {
        # THE SAME OPTION SWAY READS, not a copy of it.  The NixOS
        # `config.keymap` would work today only because ui/dvp assigns one to
        # the other; a host setting just the home-manager side would give sway
        # one layout and domicile another with nothing to say so.
        #
        # `options` goes across as the list it is -- sway and ckbcomp each join
        # it where they are written, and this is the reader that wanted a list.
        input = lib.optionalAttrs (keyboard != null) {
          keyboard = {
            xkb_layout = keyboard.layout;
            xkb_variant = keyboard.variant;
            xkb_options = keyboard.options;
          };
        };

        # Everything else is left to the module's declared defaults, which it
        # writes into the file rather than leaving out.  They are domicile's
        # own today -- checked field by field against `domicile-config` --
        # and they come from the same flake input as the compositor that
        # reads them, so there is no version skew for them to drift across.
        # Nothing compares them, though: the guard next door in domicile
        # compares option NAMES and says so.
        output.profiles =
          # The lid open and nothing plugged in.
          (profile "laptop-only" {
            laptopPanel =
              at {
                x = 0;
                y = 0;
              }
              laptopPanel;
          })
          # The travel monitor, to the right of the laptop.
          ++ (profile "portable-panel" {
            laptopPanel =
              at {
                x = 0;
                y = 0;
              }
              laptopPanel;
            portablePanel =
              at {
                x = laptopPanel.logical.width;
                y = 0;
              }
              portablePanel;
          })
          # One monitor on the desk, the laptop centered underneath it.
          ++ (profile "home-office-center" {
            laptopPanel =
              at {
                x = (center.logical.width - laptopPanel.logical.width) / 2;
                y = center.logical.height;
              }
              laptopPanel;
            center =
              at {
                x = 0;
                y = 0;
              }
              center;
          })
          # Two of the three on their sides, the laptop bottom-aligned to their
          # left: its top edge is how tall they are turned, less its own height.
          ++ (profile "home-office-right-two" {
            laptopPanel =
              at {
                x = 0;
                y = (heightOnItsSide center) - laptopPanel.logical.height;
              }
              laptopPanel;
            center =
              sideways {
                x = laptopPanel.logical.width;
                y = 0;
              }
              center;
            right =
              sideways {
                x = laptopPanel.logical.width + (widthOnItsSide center);
                y = 0;
              }
              right;
          })
          # The full desk, lid shut.  The panel is still named: a profile
          # applies only to the exact set it names, and a shut panel is still
          # connected.
          ++ (profile "home-office-full" {
            laptopPanel = off laptopPanel;
            left =
              sideways {
                x = 0;
                y = 0;
              }
              left;
            center =
              sideways {
                x = widthOnItsSide left;
                y = 0;
              }
              center;
            right =
              sideways {
                x = (widthOnItsSide left) + (widthOnItsSide center);
                y = 0;
              }
              right;
          })
          # The ultrawide, with the laptop centered underneath it.
          ++ (profile "home-office-curved" {
            laptopPanel =
              at {
                x = (curved.logical.width - laptopPanel.logical.width) / 2;
                y = curved.logical.height;
              }
              laptopPanel;
            curved =
              at {
                x = 0;
                y = 0;
              }
              curved;
          });
      };
    };
  };
}
