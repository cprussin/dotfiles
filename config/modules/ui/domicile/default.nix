# This desk, for domicile.  The sway half of it is
# config/modules/ui/kanshi/default.nix: same monitors, same arithmetic, and the
# same rule -- the first profile whose monitors are exactly what is plugged in
# wins, re-matched on every hotplug.
#
# Names are the panel's EDID string as domicile builds it, which differs from
# kanshi's in two ways: the make is the three-letter PNP id ("DEL", not "Dell
# Inc.") and a missing serial is left out rather than spelled "Unknown".  A
# name that matches nothing is not an error -- the desk just comes up unplaced.
#
# Positions assume every monitor comes up at its native mode.  kanshi pins one
# per output; a domicile profile has no mode field, so the mode arrives with
# the monitor and the compositor divides it by the scale.
#
# If a screen comes up upside down, the fix is domicile's cover-the-window.ts.
{
  config,
  lib,
  ...
}: let
  # Physical pixels and the density each panel is readable at.  `logical` is
  # what the desktop is laid out in, left fractional so that positions -- which
  # are sums of these -- don't drift a pixel per monitor across the desk.
  panel = scale: width: height: {
    inherit scale;
    logical = {
      width = width / scale;
      height = height / scale;
    };
  };

  laptop = panel 1.5 2880 1920;
  portable = panel 1.4 3840 2160;
  curved = panel 1.0 3440 1440;
  # The three on the desk are the same monitor three times over.
  external = panel 1.2 3840 2160;

  names = {
    laptop = "BOE NE135A1M-NY1";
    curved = "GSM LG ULTRAWIDE 0x0001E368";
    left = "DEL DELL U3219Q 2ZLS413";
    center = "DEL DELL U3219Q G3MS413";
    right = "DEL DELL U3219Q H8KF413";
    # kanshi says "Audio Processing Technology  Ltd Monitor demoset-1"; the
    # double space is pnp.ids' own, which is what makes the model "Monitor".
    portable = "APT Monitor demoset-1";
  };

  # Rounded outward, like the kanshi module: a monitor a pixel further out
  # leaves a gap nobody sees, where one a pixel short overlaps its neighbour --
  # and domicile has no mirroring, so an overlap is two screens claiming one
  # patch of desktop.
  at = x: y: monitor: {
    inherit (monitor) scale;
    position = [(builtins.ceil x) (builtins.ceil y)];
  };

  # A quarter turn, the same 270 the kanshi file writes for these three.
  sideways = x: y: monitor: (at x y monitor) // {transform = "rotate-270";};

  # What a monitor measures once it is stood on its side.
  turnedWidth = monitor: monitor.logical.height;
  turnedHeight = monitor: monitor.logical.width;

  profile = name: displays: {
    inherit name;
    displays =
      lib.mapAttrsToList
      (key: placement: placement // {display = names.${key};})
      displays;
  };

  keyboard = config.primary-user.home-manager.keymap;
in {
  primary-user.home-manager = {
    imports = [config.flake-inputs.domicile.homeManagerModules.default];

    programs.domicile = {
      # `mkDefault` so a host on this profile can turn it off: a bare `true`
      # would make its `false` a conflict rather than an override.
      enable = lib.mkDefault true;
      settings = {
        # The same option sway reads, not a copy of it: a host setting only one
        # of the two would give sway one layout and domicile another with
        # nothing to say so.  `options` goes across as the list it is.
        input = lib.optionalAttrs (keyboard != null) {
          keyboard = {
            xkb_layout = keyboard.layout;
            xkb_variant = keyboard.variant;
            xkb_options = keyboard.options;
          };
        };

        output.profiles = [
          # The lid open and nothing plugged in.
          (profile "laptop-only" {
            laptop = at 0 0 laptop;
          })

          # The travel monitor, to the right of the laptop.
          (profile "portable-panel" {
            laptop = at 0 0 laptop;
            portable = at laptop.logical.width 0 portable;
          })

          # One monitor on the desk, the laptop centred underneath it.
          (profile "home-office-center" {
            laptop =
              at
              ((external.logical.width - laptop.logical.width) / 2)
              external.logical.height
              laptop;
            center = at 0 0 external;
          })

          # Two of the three on their sides, the laptop bottom-aligned to their
          # left: its top edge is how tall they are turned, less its own height.
          (profile "home-office-right-two" {
            laptop = at 0 ((turnedHeight external) - laptop.logical.height) laptop;
            center = sideways laptop.logical.width 0 external;
            right = sideways (laptop.logical.width + (turnedWidth external)) 0 external;
          })

          # The full desk, lid shut.  The panel is still named: a profile
          # applies only to the exact set it names, and a shut panel is still
          # connected.
          (profile "home-office-full" {
            laptop = {enabled = false;};
            left = sideways 0 0 external;
            center = sideways (turnedWidth external) 0 external;
            right = sideways (2 * (turnedWidth external)) 0 external;
          })

          # The ultrawide, with the laptop centred underneath it.
          (profile "home-office-curved" {
            laptop =
              at
              ((curved.logical.width - laptop.logical.width) / 2)
              curved.logical.height
              laptop;
            curved = at 0 0 curved;
          })
        ];
      };
    };
  };
}
