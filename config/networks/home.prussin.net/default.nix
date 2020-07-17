let
  mkMachine = targetHost: { config, ... }: {
    deployment = {
      inherit targetHost;
      targetUser = config.primary-user.name;
      sshOptions = [ "-A" ];
    };
    imports = [ (../../machines + "/${targetHost}") ];
  };
in

{
  network.description = "Home servers";
  crux = mkMachine "crux";
  lyra = mkMachine "lyra";
  orion = mkMachine "orion";
}
