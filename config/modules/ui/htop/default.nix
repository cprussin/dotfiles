{...}: {
  primary-user.home-manager.programs.htop = {
    enable = true;
    settings = {
      hide_userland_threads = true;
      tree_view = true;
      show_program_path = false;
      highlight_base_name = true;
    };
  };
}
