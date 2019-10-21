{ writeScript, bash, nix, config }:

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

    testers) exec $browse 'https://tenfootuiapps.netflix.com/' ;;

    jenkins) exec $browse 'https://merch.builds.test.netflix.net/view/Shakti/' ;;

    conductors) exec $browse 'https://lyra.corp.netflix.com/inspectConductors' ;;

    local) exec $browse 'https://lyra.corp.netflix.com/browse' ;;
    develop-int) exec $browse 'https://develop-int.test.netflix.com/' ;;
    int|release-int) exec $browse 'https://release-int.test.netflix.com/' ;;
    qa|develop-stage) exec $browse 'https://develop-stage.netflix.com' ;;
    stage|release-stage) exec $browse 'https://release-stage.netflix.com' ;;

    dvds) exec $browse 'dvd-www-test-baseline-stable.eng.dvdco.netflix.com' ;;

    shell) exec ${config.terminal} -e ${bash}/bin/sh -c "cd $SHAKTI_SRC && $nixShell" ;;

    *) exec ${config.terminal} -e ${bash}/bin/sh -c "cd $SHAKTI_SRC && $nixShell --run 'shakti $*'" ;;
  esac
''
