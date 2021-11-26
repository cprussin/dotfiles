{ ... }:

{
  imports = [
    ../base
    ../../modules/ui/session
  ];

  primary-user.home-manager.default-terminal.enableApplication = false;
  enableSshdAtBoot = true;
  enableSshdAtInitrd = true;
}
