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
    switch = "10.42.0.2";
    ap1 = "10.42.0.3";
    ap2 = "10.42.0.4";
    ap3 = "10.42.0.5";

    crux = "10.42.1.1";

    camera-backyard = "10.42.2.1";
    camera-grill = "10.42.2.2";
    camera-driveway = "10.42.2.3";
    camera-front-steps = "10.42.2.4";
    camera-garage = "10.42.2.5";
    camera-side = "10.42.2.6";
    camera-laundry-door = "10.42.2.7";
    camera-front-door = "10.42.2.8";

    circinus = "10.42.3.1";
    bond-hub = "10.42.3.2";

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

  wireguard6 = mkNetwork "fc42::/48" {
    crux = "fc42::1";
    lyra = "fc42::1:0:0";
    pegasus = "fc42::1:0:1";
    andromeda = "fc42::1:0:f001";
    steam-deck = "fc42::1:1:0";
    printotron = "fc42::1:1:1";
    "5428-la-forest-drive-ipad" = "fc42::1:1:2";
    shauna-computer = "fc42::2:0:0";
    shauna-phone = "fc42::2:0:1";
    shauna-computer-2 = "fc42::2:0:2";
    reece-computer = "fc42::3:0:0";
    mom-vm = "fc42::3:0:1";
  };

  wireguard4 = mkNetwork "10.43.0.0/16" {
    crux = "10.43.0.1";
    lyra = "10.43.1.0";
    pegasus = "10.43.1.1";
    andromeda = "10.43.1.100";
    steam-deck = "10.43.2.0";
    printotron = "10.43.2.1";
    "5428-la-forest-drive-ipad" = "10.43.2.2";
    shauna-computer = "10.43.3.0";
    shauna-phone = "10.43.3.1";
    shauna-computer-2 = "10.43.3.2";
    reece-computer = "10.43.4.0";
    mom-vm = "10.43.4.1";
  };
}
