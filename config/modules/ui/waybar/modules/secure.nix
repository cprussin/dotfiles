{
  writeShellScript,
  config,
}: {
  name = "custom/secure";

  config = {
    exec = writeShellScript "secure" ''
      if zpool list ${config.primary-user.secure.pool} >/dev/null 2>&1
      then
        echo ""
      fi
    '';

    interval = 1;
  };
}
