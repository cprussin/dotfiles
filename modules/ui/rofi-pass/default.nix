{ pkgs, config, ... }:

{
  home-manager.users.${config.primaryUserName} = { ... }: {
    xdg.configFile."rofi-pass/config".text = ''
      _rofi() {
        ${pkgs.launcher}/bin/prompt \
          -theme-str "textbox-prompt-colon { enabled: false; }" \
          -columns 3 \
          -no-auto-select \
          "$@"
      }

      PASSWORD_STORE_DIR=${config.secure.passwords}
      URL_field='URL'
      USERNAME_field='Username'
      AUTOTYPE_field='Autotype'
      default_autotype='Username :tab pass'
    '';
  };
}
