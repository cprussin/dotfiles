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

  # Docker-compatible API, rootless: each user's socket-activated
  # `podman.socket` (shipped in the package).  Not `dockerSocket`, which is
  # rootful and would hand root to anything running as the user, CI included.
  #
  # DOCKER_HOST through nushell, the primary user's shell: it doesn't source
  # /etc/profile, and pam_env runs before XDG_RUNTIME_DIR is set.
  systemd.user.sockets.podman.wantedBy = ["sockets.target"];
  primary-user.home-manager.programs.nushell.extraEnv = ''
    load-env (if "XDG_RUNTIME_DIR" in $env {
      {DOCKER_HOST: $"unix://($env.XDG_RUNTIME_DIR)/podman/podman.sock"}
    } else {
      {}
    })
  '';
}
