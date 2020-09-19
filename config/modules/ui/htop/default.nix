{ ... }:

{
  primary-user.home-manager.programs.htop = {
    enable = true;
    hideUserlandThreads = true;
    treeView = true;
    showProgramPath = false;
    highlightBaseName = true;
    #vimMode = true;
  };
}
