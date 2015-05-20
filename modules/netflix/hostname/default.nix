{ config, ... }:

{
  networking.extraHosts = "127.0.0.1 ${config.networking.hostName}.corp.netflix.com";
}
