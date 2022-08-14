{}: let
  net-prefix = "10.42";
in {
  subnet = "255.255.0.0";

  net-hardware = let
    prefix = "${net-prefix}.0";
  in {
    id = "${prefix}.0";
    router = "${prefix}.1";
  };

  wireguard = let
    prefix = "${net-prefix}.1";
  in {
    id = "${prefix}.0";
    crux = "${prefix}.1";
    gemini = "${prefix}.10";
    pegasus = "${prefix}.11";
    shauna-computer = "${prefix}.20";
    shauna-phone = "${prefix}.21";
  };

  nixos-containers = let
    prefix = "${net-prefix}.2";
  in {
    id = "${prefix}.0";
    coredns = "${prefix}.1";
    home-assistant-proxy = "${prefix}.2";
  };

  nixos-container-host-addresses = let
    prefix = "${net-prefix}.3";
  in {
    coredns = "${prefix}.1";
    home-assistant-proxy = "${prefix}.2";
  };

  oci-containers = let
    prefix = "${net-prefix}.4";
  in {
    id = "${prefix}.0";
    crux = "${prefix}.1";
    home-assistant = "${prefix}.2";
  };

  iot = let
    prefix = "${net-prefix}.5";
  in {
    aidens-room-lights = "${prefix}.1";
    bar-lights = "${prefix}.2";
    bodhis-room-lights = "${prefix}.3";
    dining-room-lights = "${prefix}.4";
    entertainment-center-lights = "${prefix}.5";
    family-room-lights = "${prefix}.6";
    guest-room-lights = "${prefix}.7";
    hall-lights = "${prefix}.8";
    kitchen-lights = "${prefix}.9";
    living-room-lights = "${prefix}.10";
    master-bedroom-lights = "${prefix}.11";
    spare-dimmer-1 = "${prefix}.12";
    aidens-room-fan = "${prefix}.50";
    bodhis-room-fan = "${prefix}.51";
    guest-room-fan = "${prefix}.52";
    master-bedroom-fan = "${prefix}.53";
    entry-light = "${prefix}.100";
    front-porch-light = "${prefix}.101";
    spare-switch-1 = "${prefix}.102";
    plug-1 = "${prefix}.150";
    plug-2 = "${prefix}.151";
    plug-3 = "${prefix}.152";
    plug-4 = "${prefix}.153";
    plug-5 = "${prefix}.154";
    dual-plug-1 = "${prefix}.200";
    dual-plug-2 = "${prefix}.201";
    dual-plug-3 = "${prefix}.202";
    dual-plug-4 = "${prefix}.203";
  };

  dhcp = let
    prefix = "${net-prefix}.255";
  in {
    id = "${prefix}.0";
  };
}
