{ ... }:

{
  imports = [
    ../base
    ../../modules/ui/session
  ];

  promptColor = "red";
  enableTermiteApplicationConfig = false;
  enableSshdAtBoot = true;
}
