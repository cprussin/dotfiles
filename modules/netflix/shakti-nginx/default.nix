{ pkg, lib, config, ... }:

{
  options.shakti.nginxConfigPath = lib.mkOption {
    type = lib.types.string;
    default = "/home/${config.primaryUserName}/Projects/shakti/Sources/shakti/packages/shakti-build/tasks/nginx/nginx-app-generated.conf";
    description = ''
      A string containing the path to the generated nginx config file.
    '';
  };

  config = {
    services.nginx = {
      enable = true;
      appendHttpConfig = "include ${config.shakti.nginxConfigPath};";
    };
    systemd.services.nginx.wantedBy = lib.mkForce [ ];

    sudoCmds = [
      "/run/current-system/sw/bin/systemctl start nginx"
      "/run/current-system/sw/bin/systemctl stop nginx"
      "/run/current-system/sw/bin/systemctl restart nginx"
    ];
  };
}
