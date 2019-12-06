{ pkgs, config, ... }:

{
  primary-user.home-manager.xdg.configFile."rofi-pass/config".text = ''
    _rofi() {
      ${pkgs.rofi}/bin/rofi \
        -dmenu -i \
        -show run \
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
}
