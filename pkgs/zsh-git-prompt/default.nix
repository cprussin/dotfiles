{ stdenv, makeWrapper, python, git }:

let
  sources = import ../../sources.nix;
in

stdenv.mkDerivation rec {
  name = "zsh-git-prompt";
  src = sources.zsh-git-prompt;
  buildInputs = [ makeWrapper ];
  phases = [ "unpackPhase" "installPhase" "fixupPhase" ];
  installPhase = ''
    install -m755 -D $src/zshrc.sh $out/share/${name}/${name}.zsh
    install -m644 -D $src/gitstatus.py $out/share/${name}/gitstatus.py
    substituteInPlace $out/share/${name}/${name}.zsh \
      --replace 'py_bin=''${ZSH_GIT_PROMPT_PYBIN:-"python"}' 'py_bin="${python.interpreter}"' \
      --replace 'git ' '${git}/bin/git '
  '';
}
