_: {
  virtualisation = {
    containers = {
      enable = true;
      registries.search = ["docker.io"];
    };
    podman = {
      enable = true;
      dockerCompat = true;
    };
  };
}
