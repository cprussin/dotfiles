{
  lib,
  config,
  pkgs,
  ...
}: let
  sources = import ../../../sources.nix;
in {
  imports = [
    "${sources.home-manager}/nix-darwin"

    ../../../modules/darwin.nix

    ../../modules/data/git
    ../../modules/data/session-vars
    ../../modules/security/gpg
    ../../modules/security/ssh
    ../../modules/system/nix
    ../../modules/system/stateVersion
    ../../modules/system/zone/pacific
    ../../modules/ui/bash
    ../../modules/ui/nushell
    ../../modules/ui/direnv
    ../../modules/ui/emacs
    ../../modules/ui/fzf
    ../../modules/ui/less
    ../../modules/ui/readline
    ../../modules/ui/zsh

    ./nix-config.nix
    ./linkapps.nix
  ];

  system.stateVersion = lib.mkForce 4;
  environment = {
    systemPackages = [pkgs.nushell];
    shells = [pkgs.nushell];
  };
  services = {
    lorri.enable = true;
    emacs = {
      enable = true;
      inherit (config.primary-user.home-manager.programs.emacs) package;
    };
  };
  programs = {
    bash.enable = true;
    zsh.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };
  primary-user = {
    name = "connor.prussin";
    home-manager = {
      services = {
        emacs.enable = lib.mkForce false;
        gpg-agent.enable = lib.mkForce false;
      };
      programs = {
        gpg = {
          publicKeys = lib.mkForce [
            {
              source = ./gpg-key.asc;
              trust = "ultimate";
            }
          ];
        };
        git = {
          userEmail = lib.mkForce "connor.prussin@verkada.com";
          signing.key = lib.mkForce "0xEB181FDA";
        };
        emacs.emacs-rc = {
          browse = "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome";
          pngnq = "/";
          initExtra = ''
            (setq insert-directory-program "${pkgs.coreutils}/bin/ls")

            (use-package exec-path-from-shell
              :config
              (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
              (add-to-list 'exec-path-from-shell-variables "GPG_TTY")
              (exec-path-from-shell-initialize))

            (use-package frame
              :config
              (add-to-list 'default-frame-alist '(font . "monospace-14"))
              (add-to-list 'default-frame-alist '(top . 0))
              (add-to-list 'default-frame-alist '(left . 0))
              (add-to-list 'default-frame-alist '(height . 100))
              (add-to-list 'default-frame-alist '(width . 200)))
          '';
        };
        ssh = {
          userKnownHostsFile = lib.mkForce "${./known_hosts}";
          matchBlocks = lib.mkForce {};
        };
        nushell = {
          extraEnv = lib.optionalString (config ? environment) ''
            $env.PATH = ${builtins.replaceStrings
            [
                "$USER"
                "$HOME"
            ]
            [
              config.primary-user.home-manager.home.username
              config.primary-user.home-manager.home.homeDirectory
            ]
              config.environment.systemPath}
          '';
        };
      };
      home = {
        file.".gnupg/sshcontrol".text = "A135B1AA6D3DB63DAA12DECF970441475072738D\n";
        packages = lib.mkForce [
          config.primary-user.home-manager.programs.emacs.package
          pkgs.lorri
        ];
      };
    };
  };
}
