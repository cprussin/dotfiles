{lib}: let
  subnets = {
    "8" = "255.0.0.0";
    "9" = "255.128.0.0";
    "10" = "255.192.0.0";
    "11" = "255.224.0.0";
    "12" = "255.240.0.0";
    "13" = "255.248.0.0";
    "14" = "255.252.0.0";
    "15" = "255.254.0.0";
    "16" = "255.255.0.0";
    "17" = "255.255.128.0";
    "18" = "255.255.192.0";
    "19" = "255.255.224.0";
    "20" = "255.255.240.0";
    "21" = "255.255.248.0";
    "22" = "255.255.252.0";
    "23" = "255.255.254.0";
    "24" = "255.255.255.0";
    "25" = "255.255.255.128";
    "26" = "255.255.255.192";
    "27" = "255.255.255.224";
    "28" = "255.255.255.240";
    "29" = "255.255.255.248";
    "30" = "255.255.255.252";
  };

  prefixLength = cidrId: builtins.elemAt (lib.splitString "/" cidrId) 1;

  subnet = cidrId: subnets."${prefixLength cidrId}";

  mkAddress = cidrId: address: {
    inherit address;
    cidr = "${address}/${prefixLength cidrId}";
  };

  mkNetwork = cidrId: groups:
    {
      subnet = subnet cidrId;
      prefixLength = lib.toInt (prefixLength cidrId);
      id = mkAddress cidrId (builtins.elemAt (lib.splitString "/" cidrId) 0);
    }
    // builtins.mapAttrs (_: builtins.mapAttrs (_: mkAddress cidrId)) groups;
in {
  home = mkNetwork "10.42.0.0/16" {
    net-hardware = {
      router = "10.42.0.1";
    };

    iot = {
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
      spare-dimmer-1 = "10.42.5.12";
      aidens-room-fan = "10.42.5.50";
      bodhis-room-fan = "10.42.5.51";
      guest-room-fan = "10.42.5.52";
      master-bedroom-fan = "10.42.5.53";
      entry-light = "10.42.5.100";
      front-porch-light = "10.42.5.101";
      spare-switch-1 = "10.42.5.102";
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
  };

  wireguard = mkNetwork "10.43.0.0/24" {
    nodes = {
      crux = "10.43.0.1";
      gemini = "10.43.0.10";
      pegasus = "10.43.0.11";
      steam-deck = "10.43.0.12";
      shauna-computer = "10.43.0.20";
      shauna-phone = "10.43.0.21";
      reece-computer = "10.43.0.31";
      mom-vm = "10.43.0.32";
    };
  };

  oci-containers = mkNetwork "10.45.0.0/24" {
    nodes = {
      crux = "10.45.0.1";
      home-assistant = "10.45.0.2";
    };
  };
}
