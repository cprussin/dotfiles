let
  machineDir = ./config/machines;

  mkMachine = targetHost: { config, ... }: {
    deployment = {
      inherit targetHost;
      targetUser = config.primary-user.name;
      sshOptions = [ "-A" ];
      provisionSSHKey = false;
    };
    imports = [ "${toString machineDir}/${targetHost}" ];
  };

  all-machines =
    builtins.mapAttrs
      (machine: _: mkMachine machine)
      (builtins.readDir machineDir);
in
all-machines // {
  network = {
    description = "PrussinNet";
    storage.memory = { };
  };
}
