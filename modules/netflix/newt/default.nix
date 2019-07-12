{ config, ... }:

{
  home.file.".newt.yml".text = ''
    auto-upgrade:
      enabled: false
  '';
}
