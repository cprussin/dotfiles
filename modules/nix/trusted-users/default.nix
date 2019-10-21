{ config, ... }:

{
  nix.trustedUsers = [ "root" config.primaryUserName ];
}
