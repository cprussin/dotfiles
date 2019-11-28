{ config, ... }:

let
  module-path = root: module: "${toString root}/${module}";
  list-modules = root: builtins.attrNames (builtins.readDir (toString root));
  all-modules = root: map (module-path root) (list-modules root);
in

{
  imports = all-modules ./system;
  home-manager.users.${config.primaryUserName} = _: {
    imports = all-modules ./user;
  };
}
