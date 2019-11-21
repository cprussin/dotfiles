{ config, ... }:

{
  nixpkgs.overlays = [
    (
      self: super: {
        gnupg = self.symlinkJoin {
          name = "gnupg";
          paths = [ super.gnupg ];
          buildInputs = [ self.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/gpg \
              --set-default GNUPGHOME ${config.secure.gnupg}
            wrapProgram $out/bin/gpg2 \
              --set-default GNUPGHOME ${config.secure.gnupg}
            wrapProgram $out/bin/gpg-agent \
              --set-default GNUPGHOME ${config.secure.gnupg} \
              --add-flags "--options ~/.gnupg/gpg-agent.conf"
          '';
        };
      }
    )
  ];

  home-manager.users.${config.primaryUserName}.services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
  };
}
