{
  lib,
  pkgs,
  config,
  modulesPath,
  ...
}: let
  solarized-dark = pkgs.callPackage ../lib/color-themes/solarized/dark.nix {};
in {
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
  ];

  nixpkgs.overlays = [
    (
      import ../pkgs/dircolors-solarized/overlay.nix {
        src = config.flake-inputs.dircolors-solarized;
      }
    )
  ];

  boot.kernelPackages = pkgs.linuxPackages_6_12;

  isoImage = {
    isoBaseName = lib.mkForce "installer";
    appendToMenuLabel = " NixOS Installer";
  };

  services = {
    pcscd.enable = true;
    gpm.enable = true;
    getty = {
      greetingLine = lib.mkForce "> NixOS Installer <";
      helpLine = lib.mkForce "Do the thing!";
    };
  };

  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-curses;
    enableSSHSupport = true;
  };

  console = {
    keyMap = pkgs.runCommand "console-keymap" {} ''
      ${pkgs.ckbcomp}/bin/ckbcomp \
        -layout us \
        -option caps:escape \
        -variant dvp > "$out"
    '';
    colors = map (builtins.replaceStrings ["#"] [""]) [
      solarized-dark.background
      solarized-dark.red
      solarized-dark.green
      solarized-dark.yellow
      solarized-dark.blue
      solarized-dark.purple
      solarized-dark.cyan
      solarized-dark.foreground
      solarized-dark.grey
      solarized-dark.lightRed
      solarized-dark.lightGreen
      solarized-dark.lightYellow
      solarized-dark.lightBlue
      solarized-dark.lightPurple
      solarized-dark.lightCyan
      solarized-dark.white
    ];
  };

  environment.etc."inputrc".text = "set editing-mode vi";
}
