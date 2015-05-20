{ config, ... }:

{
  home.sessionVariables.NIX_PATH = "\${NIX_PATH}:nixlocal=${config.home.homeDirectory}/Projects/nixlocal";
}
