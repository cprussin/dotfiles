{ symlinkJoin, callPackage, makeWrapper, gnupg, secure }:

symlinkJoin {
  name = "gnupg";
  paths = [ (gnupg.override { pinentry = callPackage ./pinentry.nix {
    prompt = callPackage ../../ui/launcher/scripts/prompt.nix {};
  }; }) ];
  buildInputs = [ makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/gpg --set-default GNUPGHOME ${secure.gnupg}
    wrapProgram $out/bin/gpg2 --set-default GNUPGHOME ${secure.gnupg}
    wrapProgram $out/bin/gpg-agent --set-default GNUPGHOME ${secure.gnupg} --add-flags "--options ~/.gnupg/gpg-agent.conf"
  '';
}
