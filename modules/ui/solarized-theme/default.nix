{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (
      _: super: {
        toggle-colors = super.callPackage ./toggle-colors.nix {};
      }
    )
  ];

  home-manager.users.${config.primaryUserName}.xresources = {
    extraConfig = builtins.readFile (
      pkgs.fetchFromGitHub {
        owner = "solarized";
        repo = "xresources";
        rev = "025ceddbddf55f2eb4ab40b05889148aab9699fc";
        sha256 = "0lxv37gmh38y9d3l8nbnsm1mskcv10g3i83j0kac0a2qmypv1k9f";
      } + "/Xresources.dark"
    ) + ''
      #define S_base025 #0B5B70
    '';

    properties = {
      "*color8" = "S_base025";
    };
  };
}
