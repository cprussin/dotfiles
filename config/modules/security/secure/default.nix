{ ... }:

{
  primary-user.secure = {
    mountPoint = "/secure";
    device = "/dev/disk/by-label/secure";
    fsType = "ext4";
    options = [ "ro" ];
  };
}
