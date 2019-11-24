{ symlinkJoin, gnupg, makeWrapper }:

symlinkJoin {
  name = "gnupg";
  paths = [ gnupg ];
  buildInputs = [ makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/gpg-agent \
      --add-flags "--options ~/.gnupg/gpg-agent.conf"
  '';
}
