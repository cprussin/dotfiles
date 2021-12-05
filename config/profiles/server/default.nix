{ ... }:

{
  imports = [
    ../base
    ../../modules/ui/session
  ];

  primary-user.home-manager.default-terminal.enableApplication = false;
  home-manager.users.root.programs.starship.settings.hostname.ssh_only = false;
  enableSshdAtBoot = true;
  enableSshdAtInitrd = true;
}
