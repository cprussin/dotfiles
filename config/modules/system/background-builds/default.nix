# The nix daemon's work -- builds, downloads, copying sources into the store --
# runs at idle CPU priority, so it takes only the time the desktop leaves free
# and animations and typing stay smooth.  Under a load that keeps every CPU
# busy it stalls rather than slows; `batch` is the gentler choice if that
# bites.  Only on desktops: on a server the builds are the work.
#
# The idle IO class is very likely a no-op here (ioprio is BFQ-only, and this
# is ZFS on NVMe; see config/machines/crux/chromium-build.nix), and is kept for
# the day it is not.
_: {
  nix = {
    daemonCPUSchedPolicy = "idle";
    daemonIOSchedClass = "idle";
  };
}
