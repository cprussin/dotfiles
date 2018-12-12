{ pkgs, config, ... }:

let
  toggle-colors = pkgs.callPackage ./toggle-colors.nix {
    emacs = config.programs.emacs.finalPackage;
  };
in

{
  nixpkgs.overlays = [
    (self: super: { inherit toggle-colors; })
  ];

  xresources.extraConfig = builtins.readFile (
    pkgs.fetchFromGitHub {
      owner = "solarized";
      repo = "xresources";
      rev = "025ceddbddf55f2eb4ab40b05889148aab9699fc";
      sha256 = "0lxv37gmh38y9d3l8nbnsm1mskcv10g3i83j0kac0a2qmypv1k9f";
    } + "/Xresources.dark"
  ) + ''
    #define S_base025 #0B5B70
  '';

  xresources.properties = {
    "*color8" = "S_base025";
  };
}
