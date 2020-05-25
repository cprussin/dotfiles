let
  sources = import ../../sources.nix;
in

import ../../pkgs/zsh-git-prompt/overlay.nix { src = sources.zsh-git-prompt; }
