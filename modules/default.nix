{ ... }:

{
  imports = [ ./nixos ];
  primary-user.home-manager.imports = [ ./home-manager ];
  home-manager.users.root.imports = [ ./home-manager ];
}
