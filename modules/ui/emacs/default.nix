{ config, pkgs, ... }:

let
  build = emacs:
    pkgs.callPackage <nixpkgs/pkgs/build-support/emacs/trivial.nix> {
      inherit emacs;
    };

  zoom-frm = emacs: build emacs rec {
    pname = "zoom-frm";
    src = pkgs.fetchFromGitHub {
      owner = "cprussin";
      repo = pname;
      rev = "08dd33d2518f4f9a90c804e09baac1db74ffa58f";
      sha256 = "0m6by707jg7zjla7hl9xgma0w3m9apvf5dqdn7yf6qcz354kyqgy";
    };
  };
in

{
  nixpkgs.overlays = [
    (
      self: super: {
        emacs = (self.emacsPackagesGen super.emacs).emacsWithPackages (
          epkgs: [
            (pkgs.mu.override { emacs = super.emacs; })
            epkgs.aggressive-indent
            epkgs.cask-mode
            epkgs.company
            epkgs.company-emoji
            epkgs.company-quickhelp
            epkgs.counsel
            epkgs.counsel-projectile
            epkgs.csv-mode
            epkgs.delight
            epkgs.dhall-mode
            epkgs.dockerfile-mode
            epkgs.edit-indirect
            epkgs.editorconfig
            epkgs.emojify
            epkgs.evil
            epkgs.evil-collection
            epkgs.evil-goggles
            epkgs.evil-org
            epkgs.evil-magit
            epkgs.evil-smartparens
            epkgs.fill-column-indicator
            epkgs.flow-minor-mode
            epkgs.flx
            epkgs.flycheck
            epkgs.flycheck-haskell
            epkgs.flycheck-pos-tip
            epkgs.general
            epkgs.git-gutter
            epkgs.go-mode
            epkgs.groovy-mode
            epkgs.haskell-mode
            epkgs.helpful
            epkgs.highlight-numbers
            epkgs.hl-todo
            epkgs.imenu-list
            epkgs.indent-guide
            epkgs.ivy
            epkgs.link-hint
            epkgs.magit
            epkgs.markdown-mode
            epkgs.mmm-mode
            epkgs.mode-icons
            epkgs.neotree
            epkgs.nix-mode
            epkgs.org-bullets
            epkgs.pdf-tools
            epkgs.pkgbuild-mode
            epkgs.powerline
            epkgs.powerline-evil
            epkgs.projectile
            epkgs.psc-ide
            epkgs.purescript-mode
            epkgs.rainbow-delimiters
            epkgs.rainbow-mode
            epkgs.ranger
            epkgs.rust-mode
            epkgs.smartparens
            epkgs.solarized-theme
            epkgs.swiper
            epkgs.typescript-mode
            epkgs.unicode-fonts
            epkgs.use-package
            epkgs.web-mode
            epkgs.which-key
            epkgs.ws-butler
            epkgs.yaml-mode
            (zoom-frm super.emacs)
          ]
        );
      }
    )
  ];

  home-manager.users.${config.primaryUserName} = { ... }: {
    home.file = {
      ".emacs.d/modules" = {
        source = ./modules;
        recursive = true;
      };

      ".emacs.d/init.el".text = ''
        (setq nix-config #s(hash-table
                            test equal
                            data (
                                  "paths" #s(hash-table
                                             test equal
                                             data (
                                                   "ag" "${pkgs.silver-searcher}/bin/ag"
                                                   "browse" "${pkgs.launcher}/bin/browse"
                                                   "git" "${pkgs.git}/bin/git"
                                                   "msmtp" "${pkgs.msmtp}/bin/msmtp"
                                                   "sh" "${pkgs.bash}/bin/sh"
                                             ))
                                  "primaryFont" #s(hash-table
                                                   test equal
                                                   data (
                                                         "face" "${config.fontTheme.primaryFont.face}"
                                                         "size" "${toString config.fontTheme.primaryFont.size}"
                                                   ))
                                  )))

        (load (concat user-emacs-directory "modules/init"))
      '';
    };

    systemd.user.services.emacs-daemon = {
      Unit = {
        Description = "Emacs text editor";
        Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
      };

      Install = {
        WantedBy = [ "default.target" ];
      };

      Service = {
        Type = "forking";
        ExecStart = "${pkgs.stdenv.shell} -l -c 'exec ${pkgs.emacs}/bin/emacs --daemon'";
        ExecStop = "${pkgs.emacs}/bin/emacsclient --eval '(kill-emacs)'";
        Restart = "on-failure";
        SyslogIdentifier = "emacs-daemon";
      };
    };
  };
}
