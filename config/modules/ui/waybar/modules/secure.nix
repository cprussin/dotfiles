{ writeShellScript }:

{
  name = "custom/secure";

  config = {
    exec = writeShellScript "mount" ''
      if mount | grep /secure >/dev/null
      then
        echo 
      fi
    '';

    interval = 1;
  };
}
