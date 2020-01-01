{ pkgs, ... }:

{
  primary-user.home-manager = {
    default-terminal = {
      enable = true;
      bin = "${pkgs.termite}/bin/termite";
      pkg = pkgs.termite;
      terminfo = pkgs.termite.terminfo;
    };

    programs.termite = {
      enable = true;
      dynamicTitle = true;
      scrollbackLines = -1;
      hintsBorderWidth = "1";
      hintsPadding = 3;
      hintsRoundness = "0";
    };

    programs.zsh.initExtra = ''
      if [[ $TERM = xterm-termite ]]; then
        precmd_functions+=(__vte_prompt_command)
      fi
    '';
  };
}
