_: {
  fileSystems."/home/cprussin/Library" = {
    device = "crux.internal.prussin.net:/";
    fsType = "nfs";

    # TODO We mark this `noauto` for a few reasons:
    #   1. If this is mounted on boot it causes install-user-mountpoints to go
    #      nuts
    #   2. If the server becomes unavailable then any shells doing anything
    #      around the mount path get blocked, even if they aren't actually
    #      trying to access the library
    # There are probably good fixes for both of these issues and eventually it
    # would be a better UX to explore them and not make this noauto.
    options = ["noauto"];
  };
}
