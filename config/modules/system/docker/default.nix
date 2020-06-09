{ ... }:

{
  virtualisation.docker.enable = true;
  primary-user.extraGroups = [ "docker" ];
}
