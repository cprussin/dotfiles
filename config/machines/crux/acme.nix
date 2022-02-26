{...}: {
  security.acme = {
    email = "admin@prussin.net";
    acceptTerms = true;
  };

  systemd.services.nginx = {
    wants = [
      "import-tank.service"
    ];
    after = [
      "import-tank.service"
    ];
  };
}
