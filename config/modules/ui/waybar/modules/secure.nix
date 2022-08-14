{
  writeShellScript,
  config,
}: let
  icon = import ../icon.nix;
in {
  name = "custom/secure";

  config = {
    exec = writeShellScript "secure" ''
      if zpool list ${config.primary-user.secure.pool} >/dev/null 2>&1
      then
        echo "${icon ""}"
      fi
    '';

    interval = 1;
  };
}
