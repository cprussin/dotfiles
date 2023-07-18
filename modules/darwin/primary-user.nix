{
  config,
  lib,
  ...
}: let
  cfg = config.primary-user;
in {
  options.primary-user.name = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    default = null;
    description = "The name of the primary user account.";
  };

  imports = [
    (lib.mkAliasOptionModule ["primary-user" "home-manager"] ["home-manager" "users" cfg.name])
    (lib.mkAliasOptionModule ["primary-user" "home"] ["users" "users" cfg.name "home"])
    (lib.mkAliasOptionModule ["primary-user" "shell"] ["users" "users" cfg.name "shell"])
  ];

  config = lib.mkIf (cfg.name != null) {
    primary-user.home = "/Users/${cfg.name}";
    nix.settings.trusted-users = [cfg.name];
  };
}
