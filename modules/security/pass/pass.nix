{ symlinkJoin, makeWrapper, pass, secure }:

symlinkJoin {
  name = "pass";
  paths = [ (pass.withExtensions (exts: [ exts.pass-otp ])) ];
  buildInputs = [ makeWrapper ];
  postBuild = "wrapProgram $out/bin/pass --set-default PASSWORD_STORE_DIR ${secure.passwords} --set-default GNUPGHOME ${secure.gnupg}";
}
