_: {
  primary-user.home-manager.programs.imv = {
    enable = true;

    settings.binds = {
      n = "next";
      p = "prev";
      "<Ctrl+plus>" = "zoom 1";
      "<plus>" = "zoom 1";
      "<Ctrl+minus>" = "zoom -1";
      "<minus>" = "zoom -1";
      "<Ctrl+equal>" = "zoom actual";
      "<equal>" = "zoom actual";
    };
  };
}
