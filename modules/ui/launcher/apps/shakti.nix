{ writeScript, bash, nix, terminal }:

writeScript "shakti" ''
  #! ${bash}/bin/sh

  open=@out@/bin/open
  browse=@out@/bin/browse
  nixShell=${nix}/bin/nix-shell

  SHAKTI="$HOME/Projects/shakti"
  SHAKTI_SRC="$SHAKTI/Sources/shakti"
  SHAKTI_TODO="$SHAKTI/Notes/Todo"

  case "$1" in
    todo) exec $open $SHAKTI_TODO ;;
    edit) exec $open $SHAKTI_SRC ;;
    jenkins) exec $browse 'https://merch.builds.test.netflix.net/view/Shakti/' ;;
    *) exec ${terminal} -e ${bash}/bin/sh -c "cd $SHAKTI && $nixShell --run 'shakti $*'" ;;
esac
''
