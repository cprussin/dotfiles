{ stdenv, fetchFromGitHub, makeWrapper, python, git }:

stdenv.mkDerivation rec {
  name = "zsh-git-prompt";
  src = fetchFromGitHub {
    owner = "starcraftman";
    repo = name;
    rev = "11b83ba3b85d14c66cf2ab79faefab6d838da28e";
    sha256 = "04aylsjfb03ckw219plkzpyiq4j9g66bjxa5pa56h1p7df6pjssb";
  };
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
