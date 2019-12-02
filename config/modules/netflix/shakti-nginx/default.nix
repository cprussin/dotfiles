{ pkgs, lib, config, ... }:

{
  options.shakti.nginxConfigPath = lib.mkOption {
    type = lib.types.str;
    default = "/home/${config.primary-user.name}/Projects/shakti/Sources/shakti/packages/shakti-build/tasks/nginx/nginx-app-generated.conf";
    description = ''
      A string containing the path to the generated nginx config file.
    '';
  };

  config = {
    services.nginx = {
      enable = true;
      appendHttpConfig = "include ${config.shakti.nginxConfigPath};";
    };
    systemd.services.nginx.wantedBy = lib.mkForce [];

    primary-user.sudo-cmds = [
      "${pkgs.systemd}/bin/systemctl start nginx"
      "${pkgs.systemd}/bin/systemctl stop nginx"
      "${pkgs.systemd}/bin/systemctl restart nginx"
    ];
  };
}
