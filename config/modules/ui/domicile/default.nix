# The same desk as kanshi, written for domicile.
#
# config/modules/ui/kanshi/default.nix is this desk under sway, and the two
# are deliberately the same arithmetic from the same monitor records: a
# profile is chosen by which monitors are plugged in, each one gets a scale, a
# turn and a corner, and the compositor re-matches on every hotplug.  What
# differs is the file format and two things that are not cosmetic: what a
# profile matches on, and that it cannot pin a mode.  Both below.
#
# The sway half of that file does two more things this cannot yet: it assigns
# workspaces to outputs and runs `swaymsg` on a profile change to move them.
# Domicile has no workspace concept to assign, so there is nothing here that
# corresponds and nothing missing.
#
#
# WHAT DOMICILE MATCHES ON, AND WHY IT IS NOT THE EDID STRING.
#
# kanshi matches `"Dell Inc. DELL U3219Q 2ZLS413"` -- make, model and serial,
# straight off the EDID.  Domicile cannot: on a tty the engine holds DRM
# master and is the only thing that reads the connectors, and what it sends
# the compositor is `display::Display::id()` -- an opaque int64 ozone derives
# from the EDID.  The compositor names the output `drm-<that id>` and that is
# the whole identity a profile has to match on.
#
# It is stable across a hotplug and it does tell the three identical U3219Qs
# apart, because the id carries the serial.  What it does not do is say WHICH
# IS WHICH: that has to be read off a running desktop once and written down
# here.  The compositor logs the display list it takes up, one `drm-<id>` per
# monitor with its mode; nothing in this repository starts domicile as a
# service yet, so where that log goes depends on how it was started.
#
# So the ids live in `ui.domicile.displays` and default to null, and a profile
# is emitted only when every monitor it names has one.  Set none and this
# writes a config with no profiles, which is valid and leaves the monitors
# wherever the engine put them.  Fill them in per machine as they are
# discovered.
#
# This is temporary.  `DisplaySnapshot::display_name()` has the EDID string
# and `display::Display` has a `label` field to carry it in, which is the same
# route the panel's millimetres already take across the C ABI; once that
# lands, a profile matches what kanshi matches and this option goes away.
#
#
# EVERY POSITION HERE ASSUMES A MODE NOTHING PINS.
#
# kanshi sets one per output -- `mode = "3840x2160@60Hz"` and so on.  A
# domicile profile has no mode field at all: the mode arrives with the monitor
# and the compositor divides it by the scale to get the logical size, so the
# sizes the positions below are sums of are the hardware's rather than the
# config's.
#
# Not a bug today, because the modeset driver lights every connector that
# reports a mode at its native mode, which is the mode kanshi pins anyway --
# one that reports none is skipped rather than lit.  It is what would make
# this desk come up wrong if a monitor ever negotiated something else, and
# there would be nothing here to correct it with.
#
#
# NOTHING PASSES THIS FILE TO ANYTHING YET.
#
# `domicile` starts its compositor without a `--config`, so the file is
# written and read by nobody -- docs/WRITING-A-SHELL.md in the domicile repo
# calls that a gap rather than a decision.  It is written anyway because the
# layout is the part that takes a desk to work out, and it should be ready and
# reviewed before the wiring lands rather than after.
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.ui.domicile;

  # Generated rather than `builtins.toJSON`, which writes the whole desk as
  # one line.  Domicile says outright that this is not a file a person edits,
  # but it is one somebody has to read the first time a monitor lands in the
  # wrong place, and `ui/sunshine` already reaches for a `formats` generator
  # for the same reason.
  settings = pkgs.formats.json {};

  # The panels themselves, in physical pixels and at the density each is
  # readable at.  The same six records kanshi's file opens with; `logical` is
  # what the desktop is laid out in, which is what every position below is
  # measured in.
  #
  # Kept as floats rather than rounded here, exactly as the kanshi module
  # keeps them: the positions are sums of these and rounding each one first
  # would drift a pixel per monitor across the desk.  Domicile does its own
  # rounding of the logical size, from the mode and the scale, so what is
  # written out is a position and a scale and never a size.
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

  # What a monitor measures once it is stood on its side, which is the one
  # thing a transform changes about the arithmetic -- everything else it
  # changes is pixels.  Both halves are used: the rows below step sideways
  # past turned monitors by the first, and bottom-align the laptop against one
  # by the second.
  widthOnItsSide = monitor: monitor.logical.height;
  heightOnItsSide = monitor: monitor.logical.width;

  at = position: monitor: {
    inherit (monitor) scale;
    position = [(outward position.x) (outward position.y)];
  };

  sideways = position: monitor:
    (at position monitor) // {transform = "rotate-270";};

  off = _: {enabled = false;};

  # Whole pixels, because a position is an integer in the config, and rounded
  # outward for the reason the kanshi module rounds the same sums that way: a
  # monitor placed a pixel further out leaves a one-pixel gap, which the page
  # spans and nobody sees, while one placed a pixel short overlaps its
  # neighbour -- and domicile has no mirroring, so an overlap is two screens
  # claiming one patch of desktop.
  #
  # No sum on this desk actually lands between two pixels, and not because
  # the scales are kind: portablePanel's 1.4 gives it 2742.857 by 1542.857,
  # and it is the one monitor here nothing is ever placed relative to.  Every
  # size that IS a term in somebody's position -- 1.5 and 1.2 and 1.0 into
  # their modes -- happens to come out whole.  Add a monitor to the right of
  # the travel panel and this stops being true on the first sum.
  outward = builtins.ceil;

  # Every monitor this desk has, so that a key which is not one of them is
  # caught rather than read as a monitor nobody has plugged in.
  monitors = {
    inherit laptopPanel portablePanel curved left center right;
  };

  # What the compositor calls a monitor, or null where nobody has said yet.
  #
  # `or null` rather than a default attrset of nulls: an `attrsOf` option that
  # is set REPLACES its default, so a machine that names two of the six leaves
  # the other four absent rather than null, and reading them directly is an
  # eval error on the one file that is supposed to be filled in gradually.
  named = key: cfg.displays.${key} or null;

  # One profile, or nothing at all when a monitor it names has no id yet.
  # Dropping it whole is the point: a profile that quietly left out the
  # monitor it could not name would match a smaller desk and put the whole
  # arrangement up on it.
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
    # Shaped rather than any string, because the failure it prevents is the
    # silent one.  A wrong-but-well-formed name -- the bare id with no prefix,
    # or sway's `DP-1` -- simply matches no monitor, and no profile matching
    # is not an error to domicile: it leaves the monitors where the engine put
    # them and logs nothing wrong.  The desk comes up unplaced and looks like
    # this file was never read.
    #
    # The id is signed, and `DrmScreen` hands out `kDefaultDisplayId` for a
    # machine with nothing plugged in, so the sign is not assumed away here.
    type = lib.types.attrsOf (lib.types.nullOr (lib.types.strMatching "drm--?[0-9]+"));
    default = {};
    example = {
      laptopPanel = "drm-1";
      center = "drm-92";
    };
    description = ''
      What the compositor calls each monitor this desk's profiles place.  One
      of ${lib.concatStringsSep ", " (lib.attrNames monitors)}; a monitor left
      out is one whose name nobody has said yet.

      The name is `drm-<id>`, where the id is what ozone derives from the
      panel's EDID -- stable across a hotplug, and readable off the display
      list the compositor logs.  A profile naming a monitor that is still
      unset is not written out at all, so an incomplete answer here costs
      profiles rather than producing wrong ones.
    '';
  };

  config = {
    assertions = let
      unknown = lib.subtractLists (lib.attrNames monitors) (lib.attrNames cfg.displays);
      # Unset monitors dropped first, and not as tidying: `nullOr` is the
      # point of this option, so stubbing the four unknown ones as `null` is
      # the natural way to write a partly-answered desk.  Two of those are not
      # two monitors sharing a name -- and left in, they would be reported as
      # one, in a message that dies coercing `null` to a string somewhere the
      # user cannot see their own config in the trace.
      ids = lib.filter (id: id != null) (lib.attrValues cfg.displays);
      repeated = lib.unique (lib.filter (id: lib.count (other: other == id) ids > 1) ids);
      # One line, because `''` keeps the source's own line breaks and this is
      # the one string somebody reads when they have mistyped something.
      sentence = lib.concatStringsSep " ";
      list = lib.concatStringsSep ", ";
      places = "It places ${list (lib.attrNames monitors)}.";
    in [
      # A key that is not a monitor is a typo, and a silent one: it would name
      # no profile, so the profiles it was meant for would never be written
      # and the desk would come up unplaced with nothing to say why.
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
      # Two monitors on one name is easy to write here -- left, center and
      # right are the same panel three times and the lines are a copy each --
      # and expensive to get wrong there: domicile refuses a profile that
      # places one display twice, and refusing a profile refuses the whole
      # file, so the four profiles that were fine go with it.
      {
        assertion = repeated == [];
        message = sentence [
          "ui.domicile.displays gives"
          (
            if lib.length repeated == 1
            then "${list repeated} to more than one monitor."
            else "each of ${list repeated} to more than one monitor."
          )
          "Each one is a different panel, and domicile refuses the whole"
          "config file over a profile that places one display twice."
          places
        ];
      }
    ];

    primary-user.home-manager.xdg.configFile."domicile/domicile.json".source = settings.generate "domicile.json" {
      # `output` and nothing else: the keyboard, the nested size and the rest
      # take domicile's own defaults, which already carry the dvp layout and
      # the caps/escape swap this desk uses.
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
        # Two of the three on their sides, the laptop to their left and
        # bottom-aligned against them: the panel's top edge is how tall they
        # are once turned, less its own height.
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
        # The full desk: three on their sides in a row, and the laptop dark
        # because the lid is shut.  It is still named, because a profile
        # applies only to the exact set of monitors it names and the panel is
        # connected whether or not anyone can see it.
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
}
