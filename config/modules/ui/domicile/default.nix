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
# WHAT DOMICILE MATCHES ON, WHICH IS NOW NEARLY WHAT KANSHI MATCHES ON.
#
# kanshi matches `"Dell Inc. DELL U3219Q 2ZLS413"` -- make, model and serial
# off the EDID.  Domicile carries the same three now, so a profile names a
# monitor the way it is labelled rather than by `drm-<id>`, the opaque int64
# ozone derives from the same EDID.  Either still works; the id is what a
# monitor with nothing to say about itself has to be named.
#
# TWO DIFFERENCES FROM THE STRING IN THE KANSHI FILE NEXT DOOR, and both bite
# silently -- a name that matches nothing is not an error to domicile.  It
# leaves the monitors where the engine put them and logs nothing wrong, so the
# desk comes up unplaced and looks like this file was never read.
#
#   The make is the three-letter PNP id, not the vendor name.  "DEL", not
#   "Dell Inc.".  An EDID holds the id; the vendor name behind it is hwdata's
#   pnp.ids, which libdisplay-info carries and Chromium does not.
#
#   A missing serial is left out, not spelled "Unknown".  sway writes that
#   word where it found none; domicile writes nothing and the name is the two
#   parts that are left.
#
# What does NOT differ is the numeric serial.  The LG below is named
# `0x0001E368` by sway because libdisplay-info fell back to the base block's
# 32-bit field, and domicile formats that identically -- which is checkable
# from this desk's own kanshi config, and is why the fallback is there.
#
# So every name below is a derivation rather than a guess, and the vendor
# name is what each derivation turns back into three letters: hwdata's
# pnp.ids is the table libdisplay-info read them out of, and it answers in
# both directions.  Dell Inc. is DEL, LG Electronics is GSM, Audio Processing
# Technology  Ltd is APT -- each the only entry with that name -- and BOE is
# its own vendor name.  Nothing here was read off a running desktop.
#
# So nothing here is unnamed out of the box.  A monitor only ends up without
# a name when somebody sets it to `null` -- how a derived name that turns out
# wrong is taken back out -- and even then the answer is not a guess: a
# profile naming an unnamed monitor is not written out, so an incomplete
# answer costs arrangements rather than producing wrong ones.
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
# WHAT PASSES THIS FILE TO DOMICILE, AND WHAT STILL DOES NOT.
#
# The flag exists now.  `domicile --config <path> <shell>` hands the file to
# the compositor, which reads it at startup and re-reads it whenever it
# changes, so this is no longer written for nobody:
#
#     domicile --config ~/.config/domicile/domicile.json <shell>
#
# What this module does NOT do is run that command.  It writes the file and
# installs nothing -- there is no domicile package here, no session and no
# unit -- because making domicile this machine's login session is a decision
# about how the desk boots rather than about where the monitors are, and it is
# not one this file should make on the way past.  So the desk is described
# here and started by hand.
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

  # What each monitor is called, where it could be worked out from the kanshi
  # string next door rather than read off hardware.  The header says what
  # each part of that derivation rests on.
  #
  # NOT the option's `default`, and that is the whole reason this attrset
  # exists.  An `attrsOf` option that is set REPLACES its default rather than
  # merging into it, so a machine correcting one of these names would silently
  # lose the other five -- which is exactly the trap `named` below was written
  # to avoid on the other side.  Overridden per monitor instead.
  derived = {
    # sway calls this "BOE NE135A1M-NY1 Unknown".  The make survives the
    # round trip unchanged because pnp.ids gives BOE the vendor name "BOE" --
    # the same three letters, not a missing entry -- and "Unknown" is the word
    # sway writes where libdisplay-info found no serial at all, neither a
    # descriptor nor a non-zero number.  Domicile writes nothing there.
    laptopPanel = "BOE NE135A1M-NY1";

    # "Dell Inc." is hwdata's name for the maker whose EDIDs say DEL.
    left = "DEL DELL U3219Q 2ZLS413";
    center = "DEL DELL U3219Q G3MS413";
    right = "DEL DELL U3219Q H8KF413";

    # "LG Electronics" is GSM, which is the only pnp.ids entry carrying that
    # name.  `0x0001E368` is the base block's 32-bit serial, which sway prints
    # because there was no descriptor to prefer and which domicile formats the
    # same way.
    curved = "GSM LG ULTRAWIDE 0x0001E368";

    # "Audio Processing Technology  Ltd" is APT, likewise the only entry with
    # that name -- and its double space is pnp.ids' own, which is what says
    # the make ends where it does and the model is "Monitor".
    portablePanel = "APT Monitor demoset-1";
  };

  # What this desk knows a monitor as: what the machine said, else what was
  # derived, else nothing.
  #
  # `or` catches a MISSING key, not a null one, and the difference is the
  # point: a monitor set explicitly to `null` stops at the first rung rather
  # than falling through to `derived`, which is how a derived name that turns
  # out to be wrong is taken back out without having to know the right one.
  #
  # `or` rather than reading the attrs directly because neither is obliged to
  # hold every key, and reading a missing one is an eval error on the one file
  # that is supposed to be filled in gradually.
  named = key: cfg.displays.${key} or derived.${key} or null;

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
    # Either name domicile knows a monitor by: the panel's, which is what
    # `derived` above holds and what a person can write, or the output's
    # `drm-<id>`, which is what a monitor stating no make, model or serial has
    # to be called.
    #
    # Shaped rather than any string, because the failure it prevents is the
    # silent one.  A name that matches nothing is not an error to domicile --
    # it leaves the monitors where the engine put them -- so the desk comes up
    # unplaced and looks like this file was never read.
    #
    # A PANEL NAME IS NOT ALWAYS THREE PARTS, which is the whole constraint on
    # how strict this can be.  `DisplayNameFrom` joins the make, the model and
    # the serial that are there and no more, and it drops the make outright
    # when the product code is not a PNP id -- so `DELL U3219Q 2ZLS413` and
    # `DEL DELL U3219Q` and a bare `DEL` are all names the compositor
    # advertises.  Hence: three capitals on their own, or two or more words
    # separated by single spaces.
    #
    # SO IT DOES NOT CATCH EITHER MISTAKE THE HEADER IS ABOUT.  A kanshi
    # string pasted from the file next door is words separated by spaces too,
    # and no shape tells a vendor name from a model when a name may
    # legitimately begin with either.  Three of this desk's four kanshi
    # strings pass here: both Dells and the LG on their vendor names, and the
    # laptop panel on its trailing `Unknown`, which no shape ever caught
    # because a serial is a word like any other.  Only the APT one fails, and
    # only on the double space in its vendor name, which is an accident
    # rather than a check.  What the shape does catch is the two
    # single-word mistakes -- the bare id with no `drm-` in front of it, and
    # sway's connector name (`DP-1`) -- and any stray whitespace, which is how
    # a name copied out of a log arrives.  The cost of the first clause is a
    # one-word name that is not a PNP id: a panel stating only a model has to
    # be named `drm-<id>`, because nothing tells it apart from `DP-1`.
    #
    # The id is signed: `DrmScreen` hands out `kDefaultDisplayId` for a machine
    # with nothing plugged in, so the sign is not assumed away here.
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
    assertions = let
      unknown = lib.subtractLists (lib.attrNames monitors) (lib.attrNames cfg.displays);
      # The RESOLVED names, not `cfg.displays`.  Most of these names come
      # from `derived` now, so a check that read only what was written here
      # would miss the shape the mistake actually takes: one line corrected by
      # hand, to a name three identical panels share, colliding with a name
      # nobody wrote down because it was already right.
      #
      # Unset monitors dropped, and not as tidying: `nullOr` is the point of
      # this option, so stubbing an unknown one as `null` is the natural way
      # to write a partly-answered desk.  Two of those are not two monitors
      # sharing a name -- and left in, they would be reported as one, in a
      # message that dies coercing `null` to a string somewhere the user
      # cannot see their own config in the trace.
      ids = lib.filter (id: id != null) (map named (lib.attrNames monitors));
      repeated = lib.unique (lib.filter (id: lib.count (other: other == id) ids > 1) ids);
      # One line, because `''` keeps the source's own line breaks and this is
      # the one string somebody reads when they have mistyped something.
      sentence = lib.concatStringsSep " ";
      list = lib.concatStringsSep ", ";
      places = "It places ${list (lib.attrNames monitors)}.";
      # Which monitors a name resolved to.  Named rather than left to the
      # reader, because grepping for the name finds at most one of them: the
      # other is one this file derived and nobody wrote down.
      sharing = id: lib.attrNames (lib.filterAttrs (key: _: named key == id) monitors);
      # "left and center", or "center, left and right".  Not `list`: this goes
      # in the middle of a sentence, and a bare comma there reads as the end
      # of a clause rather than as another monitor.
      andList = keys:
        if lib.length keys < 2
        then list keys
        else "${list (lib.init keys)} and ${lib.last keys}";
      # "both" only when there are two of them.  Pasting one corrected serial
      # into two of the three identical Dell lines gets you three.
      shared = keys:
        if lib.length keys == 2
        then "both ${andList keys}"
        else "all of ${andList keys}";
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
      # file, so every profile that was fine goes with it.
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
