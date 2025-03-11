{pkgs, ...}: {
  programs = {
    nushell = {
      enable = true;
      extraConfig = ''
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/ack/ack-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/adb/adb-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/bat/bat-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/cargo/cargo-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/curl/curl-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/fastboot/fastboot-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/git/git-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/make/make-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/man/man-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/nix/nix-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/npm/npm-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/pass/extensions/pass-otp/pass-otp-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/pass/pass-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/pnpm/pnpm-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/poetry/poetry-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/rg/rg-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/rustup/rustup-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/tar/tar-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/tcpdump/tcpdump-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/yarn/yarn-v4-completions.nu *
      '';
      extraEnv = ''
        $env.config = {
          edit_mode: vi,
          show_banner: false,
          cursor_shape: {
            vi_insert: line,
            vi_normal: block,
            emacs: block
          },
          history: {
            sync_on_enter: false
          }
        }
      '';
    };

    direnv.enableNushellIntegration = true;
  };
}
