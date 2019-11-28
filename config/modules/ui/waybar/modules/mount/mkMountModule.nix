{ writeShellScript }: { path, icon }:

let
  exec = writeShellScript "mount" ''
    if mount | grep "${path}" >/dev/null
    then
      echo ${icon}
    fi
  '';
in

{
  name = "custom/mount-${builtins.replaceStrings [ "/" ] [ "" ] path}";

  config = {
    inherit exec;
    interval = 1;
  };
}
