{ pkgs, ... }:

{
  xdg.configFile."rofi-pass/config".text = ''
    _rofi() {
      ${pkgs.rofi}/bin/rofi \
        -font "DejaVuSansMono 10" \
        -color-normal "#002b36,#657b83,#002b36,#859900,#002b36" \
        -color-window "#002b36,#859900" \
        -bw 2 \
        -theme-str "textbox-prompt-colon { enabled: false; }" \
        -columns 3 \
        -i \
        -no-auto-select \
        "$@"
    }

    URL_field='URL'
    USERNAME_field='Username'
    AUTOTYPE_field='Autotype'
    default_autotype='Username :tab pass'
  '';
}
