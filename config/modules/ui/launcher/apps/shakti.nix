{ writeShellScript, nix, config, launcher }:
let
  terminal = config.primary-user.home-manager.default-terminal.bin;
in
writeShellScript "shakti" ''
  open=${launcher}/bin/open
  browse=${launcher}/bin/browse
  nixShell=${nix}/bin/nix-shell

  SHAKTI="$HOME/Projects/shakti"
  SHAKTI_SRC="$SHAKTI/Sources/shakti"

  case "$1" in
    edit) exec $open $SHAKTI_SRC ;;
    testers) exec $browse 'https://subview.prod.netflix.net/create' ;;
    jenkins) exec $browse 'https://merch.builds.test.netflix.net/view/Shakti/' ;;
    conductors) exec $browse 'https://lyra.corp.netflix.com/inspectConductors' ;;
    local) exec $browse 'https://lyra.corp.netflix.com/browse' ;;
    dvds) exec $browse 'dvd-www-test-baseline-stable.eng.dvdco.netflix.com' ;;
    shell) cd $SHAKTI_SRC && exec ${terminal} ;;
    *) cd $SHAKTI_SRC && exec ${terminal} -e "$nixShell --run 'shakti $*'" ;;
  esac
''
