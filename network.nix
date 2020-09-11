let
  mkMachine = targetHost: { config, ... }: {
    deployment = {
      inherit targetHost;
      targetUser = config.primary-user.name;
      sshOptions = [ "-A" ];
    };
    imports = [ (./config/machines + "/${targetHost}") ];
  };
in

{
  network.description = "PrussinNet";
  crux = mkMachine "crux";
  lyra = mkMachine "lyra";
  orion = mkMachine "orion";
}
