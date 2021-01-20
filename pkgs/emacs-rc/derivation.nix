{ trivialBuild, callPackage, epkgs, git }:

(trivialBuild {
  pname = "emacs-rc";
  version = "0.0.1";
  src = ./.;
  packageRequires = callPackage ./dependencies.nix { inherit epkgs; };
}).overrideAttrs (attrs: {
  nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ git ];
})
