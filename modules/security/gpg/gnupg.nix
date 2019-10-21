{ symlinkJoin, callPackage, makeWrapper, gnupg, config }:

symlinkJoin {
  name = "gnupg";
  paths = [
    (
      gnupg.override {
        pinentry = callPackage ./pinentry.nix {
          prompt = callPackage ../../ui/launcher/scripts/prompt.nix {
            inherit config;
          };
        };
      }
    )
  ];
  buildInputs = [ makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/gpg --set-default GNUPGHOME ${config.secure.gnupg}
    wrapProgram $out/bin/gpg2 --set-default GNUPGHOME ${config.secure.gnupg}
    wrapProgram $out/bin/gpg-agent --set-default GNUPGHOME ${config.secure.gnupg} --add-flags "--options ~/.gnupg/gpg-agent.conf"
  '';
}
