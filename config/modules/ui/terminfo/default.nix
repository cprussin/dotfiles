{
  lib,
  config,
  ...
}: {
  primary-user.home-manager.home.packages = lib.mkForce [
    config.primary-user.home-manager.default-terminal.pkg.terminfo
  ];
}
