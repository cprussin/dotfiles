{ ... }:

{
  imports = [
    ../base
    ../../modules/ui/session
  ];

  promptColor = "red";
  primary-user.home-manager.default-terminal.enableApplication = false;
  enableSshdAtBoot = true;
}
