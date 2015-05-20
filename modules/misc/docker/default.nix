{ config, ... }:

{
  virtualisation.docker.enable = true;

  users.users.${config.primaryUserName}.extraGroups = [ "docker" ];
}
