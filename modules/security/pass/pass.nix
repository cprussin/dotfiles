{ symlinkJoin, makeWrapper, pass, config }:

let
  pkg = passedExtensions: symlinkJoin {
    name = "pass";
    paths = [ (pass.withExtensions (exts: (passedExtensions exts) ++ [ exts.pass-otp ])) ];
    buildInputs = [ makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/pass \
        --set-default PASSWORD_STORE_DIR ${config.secure.passwords} \
        --set-default GNUPGHOME ${config.secure.gnupg}
    '';
  };
in

pkg (_: []) // {
  withExtensions = pkg;
}
