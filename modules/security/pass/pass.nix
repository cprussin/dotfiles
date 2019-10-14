{ symlinkJoin, makeWrapper, pass, secure }:

let
  pkg = passedExtensions: symlinkJoin {
    name = "pass";
    paths = [ (pass.withExtensions (exts: (passedExtensions exts) ++ [ exts.pass-otp ])) ];
    buildInputs = [ makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/pass \
        --set-default PASSWORD_STORE_DIR ${secure.passwords} \
        --set-default GNUPGHOME ${secure.gnupg}
    '';
  };
in

pkg (_: []) // {
  withExtensions = pkg;
}
