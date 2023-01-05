_: {
  systemd.services.syncthing = {
    requires = ["import-tank.service"];
    after = ["import-tank.service"];
  };
}
