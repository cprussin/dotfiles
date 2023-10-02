{pkgs, ...}: {
  programs = {
    nushell = {
      enable = true;
      extraConfig = ''
        source ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/pass/pass-completions.nu
      '';
      extraEnv = ''
        $env.config = {
          edit_mode: vi,
          show_banner: false,
          cursor_shape: {
            vi_insert: line,
            vi_normal: block,
            emacs: block
          }
        }
      '';
    };

    starship = {
      enable = true;
      settings.directory.truncate_to_repo = false;
    };
  };
}
