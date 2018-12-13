{ pkgs, config, ... }:

let

  emacs-trivial = pkgs.callPackage <nixpkgs/pkgs/build-support/emacs/trivial.nix> {
    emacs = pkgs.emacs;
  };

  zoom-frm = emacs-trivial rec {
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
  home.file = {
    ".emacs.d/modules" = {
      source = ./modules;
      recursive = true;
    };

    ".emacs.d/init.el".text = ''
      (setq paths '(("ag" . "${pkgs.silver-searcher}/bin/ag")
                    ("sh" . "${pkgs.bash}/bin/sh")))

      (load (concat user-emacs-directory "modules/init"))
    '';
  };

  programs.emacs = {
    enable = true;
    extraPackages = (epkgs: (with epkgs; [
      pkgs.mu
      aggressive-indent
      cask-mode
      company
      company-emoji
      company-quickhelp
      counsel
      counsel-projectile
      csv-mode
      delight
      dockerfile-mode
      edit-indirect
      editorconfig
      emojify
      evil
      evil-collection
      evil-goggles
      evil-org
      evil-magit
      evil-smartparens
      fill-column-indicator
      flow-minor-mode
      flx
      flycheck
      flycheck-haskell
      flycheck-pos-tip
      general
      git-gutter
      go-mode
      groovy-mode
      haskell-mode
      helpful
      highlight-numbers
      hl-todo
      imenu-list
      indent-guide
      ivy
      link-hint
      magit
      markdown-mode
      mmm-mode
      mode-icons
      neotree
      nix-mode
      org-bullets
      pdf-tools
      pkgbuild-mode
      powerline
      powerline-evil
      projectile
      psc-ide
      purescript-mode
      rainbow-delimiters
      rainbow-mode
      ranger
      smartparens
      solarized-theme
      swiper
      typescript-mode
      unicode-fonts
      use-package
      web-mode
      which-key
      ws-butler
      zoom-frm
    ]));
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
      ExecStart = "${pkgs.stdenv.shell} -l -c 'exec %h/.nix-profile/bin/emacs --daemon'";
      ExecStop = "%h/.nix-profile/bin/emacsclient --eval '(kill-emacs)'";
      Restart = "on-failure";
      SyslogIdentifier = "emacs-daemon";
    };
  };
}
