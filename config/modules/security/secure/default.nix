{ ... }:

{
  primary-user.secure = {
    mountPoint = "/secure";
    device = "/dev/disk/by-uuid/c00737cd-c285-4aeb-961d-89cb40fbf4bc";
    fsType = "ext4";
    options = [ "noauto" ];
  };
}
