{ config, lib, ... }:
let
  cfg = config.umask;
in
{
  options.umask = lib.mkOption {
    type = lib.types.attrsOf lib.types.str;
    default = { };
    description = ''
      An attrset mapping usernames to the umask to use for that user.
    '';
  };

  config = {
    systemd.services = lib.mapAttrs'
      (
        username: umask: lib.nameValuePair "home-manager-${username}" {
          serviceConfig.UMask = umask;
        }
      )
      cfg;

    home-manager.users = lib.mapAttrs
      (
        _: umask:
          let
            umaskCmd = "umask ${umask}";
          in
          {
            home.file = {
              ".profile".text = umaskCmd;
              ".bashrc".text = umaskCmd;
            };
            programs.zsh.initExtra = umaskCmd;
          }
      )
      cfg;
  };
}
