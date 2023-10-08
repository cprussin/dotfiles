{lib}: let
  mkNetwork = cidrId: hosts: let
    parts = lib.splitString "/" cidrId;
    id = builtins.elemAt parts 0;
    prefixLength = builtins.elemAt parts 1;
    mkAddress = address: {
      inherit address prefixLength;
      cidr = "${address}/${prefixLength}";
    };
  in
    {
      prefixLength = lib.toInt prefixLength;
      id = mkAddress id;
    }
    // builtins.mapAttrs (_: mkAddress) hosts;
in {
  home = mkNetwork "10.42.0.0/16" {
    router = "10.42.0.1";
    circinus = "10.42.1.1";
    aidens-room-lights = "10.42.5.1";
    bar-lights = "10.42.5.2";
    bodhis-room-lights = "10.42.5.3";
    dining-room-lights = "10.42.5.4";
    entertainment-center-lights = "10.42.5.5";
    family-room-lights = "10.42.5.6";
    guest-room-lights = "10.42.5.7";
    hall-lights = "10.42.5.8";
    kitchen-lights = "10.42.5.9";
    living-room-lights = "10.42.5.10";
    master-bedroom-lights = "10.42.5.11";
    garage-lights = "10.42.5.12";
    aidens-room-fan = "10.42.5.50";
    bodhis-room-fan = "10.42.5.51";
    guest-room-fan = "10.42.5.52";
    master-bedroom-fan = "10.42.5.53";
    entry-light = "10.42.5.100";
    front-porch-light = "10.42.5.101";
    trash-light = "10.42.5.102";
    plug-1 = "10.42.5.150";
    plug-2 = "10.42.5.151";
    plug-3 = "10.42.5.152";
    plug-4 = "10.42.5.153";
    plug-5 = "10.42.5.154";
    dual-plug-1 = "10.42.5.200";
    dual-plug-2 = "10.42.5.201";
    dual-plug-3 = "10.42.5.202";
    dual-plug-4 = "10.42.5.203";
  };

  wireguard = mkNetwork "fc42::/48" {
    crux = "fc42::1";
    gemini = "fc42::1:0:0";
    pegasus = "fc42::1:0:1";
    andromeda = "fc42::1:0:f001";
    steam-deck = "fc42::1:1:0";
    printotron = "fc42::1:1:1";
    shauna-computer = "fc42::2:0:0";
    shauna-phone = "fc42::2:0:1";
    reece-computer = "fc42::3:0:0";
    mom-vm = "fc42::3:0:1";
  };
}
