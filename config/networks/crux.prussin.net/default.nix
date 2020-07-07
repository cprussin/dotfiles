{
  network.description = "Home server";

  crux = {
    deployment = {
      targetEnv = "none";
      targetHost = "crux";
      targetPort = 22;
    };

    imports = [ ../../machines/crux ];
  };
}
