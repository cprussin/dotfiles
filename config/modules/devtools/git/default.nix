{ config, ... }:

{
  home-manager.users.${config.primaryUserName}.programs.git = {
    enable = true;
    userName = "Connor Prussin";
    userEmail = "connor@prussin.net";
    signing = {
      key = "C72452E036D53A6A";
      signByDefault = true;
    };
    extraConfig = {
      pull.rebase = true;
      color.ui = "auto";
      push = {
        default = "simple";
        gpgsign = "if-asked";
      };
      alias = {
        tree = "\"log --all --graph --pretty=format:'%C(yellow)%h %C(cyan)%ai (%ar) %Cred%d%Creset\\n        %C(bold)%an <%ae>%Creset\\n        %s\\n'\"";
        alias = "\"!git config --list | grep 'alias\\\\.' | sed 's/alias\\\\.\\\\([^=]*\\\\)=\\\\(.*\\\\)/\\\\1\\\\\t => \\\\2/' | sort\"";
        merge-log = "\"!f() { git log --stat \\\"\$1^..\$1\\\"; }; f\"";
      };
    };
  };
}
