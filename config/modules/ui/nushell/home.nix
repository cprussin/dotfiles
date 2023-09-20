_: let
  sources = import ../../../../sources.nix;
in {
  programs = {
    nushell = {
      enable = true;
      extraConfig = ''
        use ${sources.nu_scripts}/custom-completions/pass/pass-completions.nu *
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
