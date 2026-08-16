_: let
  # WirePlumber picks the default sink by taking the node with the highest
  # `priority.session` (see its `find-best-default-node.lua`), and its defaults
  # get that wrong for us in two ways:
  #
  #   - The ALSA monitor scores every analog USB sink identically: 1000 for
  #     hardware device 0, +9 for the analog profile, and +100 for sitting on
  #     the USB bus.  So the Plugable dock's analog jack ties with real USB
  #     speakers at 1109 and wins the tie on enumeration order, even though
  #     nothing is ever plugged into the jack.
  #   - The bluez monitor sets no priority at all, so Bluetooth sinks fall back
  #     to 0 and can never win against a wired sink.
  #
  # Give each class its own band instead.  Only `priority.session` is
  # overridden: `priority.driver` decides which node drives the graph clock, and
  # Bluetooth in particular makes a poor driver.
  #
  # The bands are chosen to sit around the priorities we leave alone, which on
  # lyra are 1500 for the enhanced internal speaker sink from
  # `hardware.framework.laptop13.audioEnhancement` (1009 for the raw sink behind
  # it) and ~584 for the Radeon HDMI output.
  bluetoothSinks = [{"node.name" = "~bluez_output..*";}];

  usbSinks = [{"node.name" = "~alsa_output.usb-.*";}];

  # The analog jack on the Plugable UD-6950PDZ dock is a USB audio device, so
  # the rule above would otherwise promote it alongside real USB speakers.
  # Nothing is ever plugged into that jack, so sort it below everything else,
  # including the internal speakers; it can still be selected by hand.  Its
  # capture side (an IEC958 input) gets the same treatment.
  #
  # The node name is built from the USB vendor and product strings, which is
  # brittle enough to be worth matching a few ways: `wpctl inspect` on the sink
  # shows what `node.name` and `node.description` actually came out as.
  dockNodes = [
    {"node.name" = "~alsa_output.usb-.*UD-6950.*";}
    {"node.name" = "~alsa_input.usb-.*UD-6950.*";}
    {"node.name" = "~alsa_output.usb-DisplayLink.*";}
    {"node.name" = "~alsa_input.usb-DisplayLink.*";}
    {"node.description" = "~Plugable UD-6950.*";}
  ];

  mkPriorityRule = matches: priority: {
    inherit matches;
    actions.update-props."priority.session" = priority;
  };
in {
  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    jack.enable = true;

    # Rules apply in order and later ones win, so the dock rule has to follow
    # the general USB rule that it narrows.
    wireplumber.extraConfig."50-audio-device-priorities" = {
      "monitor.alsa.rules" = [
        (mkPriorityRule usbSinks 2000)
        (mkPriorityRule dockNodes 500)
      ];
      "monitor.bluez.rules" = [(mkPriorityRule bluetoothSinks 3000)];
    };
  };
}
