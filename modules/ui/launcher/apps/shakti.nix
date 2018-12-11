{ writeScript, bash, nix }:

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
    *) exec $TERMINAL -e $SHELL -c "cd $SHAKTI && $nixShell --run 'shakti $*'" ;;
esac
''
