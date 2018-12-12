{ stdenv, fetchFromGitHub, python, git }:

stdenv.mkDerivation {
  name = "zsh-git-prompt";
  src = fetchFromGitHub {
    owner = "starcraftman";
    repo = "zsh-git-prompt";
    rev = "11b83ba3b85d14c66cf2ab79faefab6d838da28e";
    sha256 = "04aylsjfb03ckw219plkzpyiq4j9g66bjxa5pa56h1p7df6pjssb";
  };
  phases = [ "unpackPhase" "installPhase" "fixupPhase" ];
  installPhase = ''
    cp -r $src $out
     substituteInPlace $out/zshrc.sh \
       --replace 'py_bin=''${ZSH_GIT_PROMPT_PYBIN:-"python"}' 'py_bin=''${ZSH_GIT_PROMPT_PYBIN:-"${python.interpreter}"}' \
       --replace 'git ' '${git}/bin/git '
  '';
}
