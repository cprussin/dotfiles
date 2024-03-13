{
  sources ? import ./sources.nix,
  nixpkgs ? sources.nixpkgs,
  niv ? sources.niv,
  mkCli ? sources.mkCli,
}: let
  niv-overlay = self: _: {
    niv = self.symlinkJoin {
      name = "niv";
      paths = [niv];
      buildInputs = [self.makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/niv \
          --add-flags "--sources-file ${toString ./sources.json}"
      '';
    };
  };

  password-utils-overlay = self: _: {
    inherit (self.callPackage ./lib/passwords.nix {}) passwordUtils;
  };

  mkCli-overlay = import "${mkCli}/overlay.nix";

  pkgs = import nixpkgs {
    overlays = [
      niv-overlay
      password-utils-overlay
      mkCli-overlay
    ];
    config = {};
  };

  network = pkgs.callPackage ./lib/network.nix {};

  cli = pkgs.lib.mkCli "cli" {
    _noAll = true;

    systems-test = pkgs.writeShellScript "systems-test" ''
      pass="${pkgs.pass}/bin/pass"
      curl="${pkgs.curl}/bin/curl"
      jq="${pkgs.jq}/bin/jq"
      dig="${pkgs.dnsutils}/bin/dig"
      syncthing="${pkgs.syncthing}/bin/syncthing"
      xmllint="${pkgs.libxml2}/bin/xmllint"

      echo -n "Checking if crux is up on wireguard ipv6...  "
      if ping -q -c 1 -W 1 ${network.wireguard6.crux.address} >/dev/null; then
          echo "✅"
      else
          echo "❌"
          echo "Crux (ipv6) is down!"
          exit 1
      fi

      echo -n "Checking if crux is up on wireguard ipv4...  "
      if ping -q -c 1 -W 1 ${network.wireguard4.crux.address} >/dev/null; then
          echo "✅"
      else
          echo "❌"
          echo "Crux (ipv4) is down!"
          exit 1
      fi

      echo -n "Checking if dns is up...  "
      DIG_RES="$($dig +short AAAA ${network.wireguard6.crux.address} crux.internal.prussin.net)"
      if [ "$DIG_RES" == "${network.wireguard6.crux.address}" ]; then
          echo "✅"
      else
          echo "❌"
          echo "DNS is not working correctly!"
          exit 1
      fi

      echo -n "Checking if home assistant is running...  "
      HASS_KEY="$($pass show "Connor/Home/Home Assistant" | grep "^API Key: " | sed "s/^API Key: //")"
      HASS_RES=$($curl -s -H "Authorization: Bearer $HASS_KEY" -H "Content-Type: application/json" https://home-assistant.internal.prussin.net/api/)
      if [ "$(echo "$HASS_RES" | $jq -r '.message' 2>/dev/null)" == "API running." ]; then
          echo "✅"
      else
          echo "❌"
          echo "Home assistant is down!"
          exit 1
      fi

      echo -n "Checking if library (http) is running...  "
      LIBRARY_RES=$(curl -s https://library.internal.prussin.net)
      if [ "$(echo "$LIBRARY_RES" | $xmllint --html --xpath 'string(/html/head/title)' -)" == "Index of /" ]; then
          echo "✅"
      else
          echo "❌"
          echo "Library (http) is down!"
          exit 1
      fi

      echo -n "Checking if library (ftp) is running...  "
      curl -s ftp://library.internal.prussin.net 2>&1 >/dev/null
      if [ $? -eq 0 ]; then
          echo "✅"
      else
          echo "❌"
          echo "Library (ftp) is down!"
          exit 1
      fi

      echo -n "Checking if matrix (client) is running...  "
      MATRIX_CLIENT_RES=$(curl -s https://matrix.internal.prussin.net/health)
      if [ "$(echo "$MATRIX_CLIENT_RES")" == "OK" ]; then
          echo "✅"
      else
          echo "❌"
          echo "Matrix (client) is down!"
          exit 1
      fi

      echo -n "Checking if matrix (federation) is running...  "
      MATRIX_FEDERATION_RES=$(curl -s https://matrix.prussin.net:8448/health)
      if [ "$(echo "$MATRIX_FEDERATION_RES")" == "OK" ]; then
          echo "✅"
      else
          echo "❌"
          echo "Matrix (federation) is down!"
          exit 1
      fi

      # TODO check syncthing connections
      # echo -n "Checking syncthing connections...  "
      # SYNCTHING_RES=$($syncthing cli --home=/var/lib/syncthing/.config/syncthing show connections)
      # for host in $(ls ''${PASSWORD_STORE_DIR-~/.password-store}/Connor/Infrastructure/syncthing | sed "/$(hostname)/d"); do pass show Connor/Infrastructure/syncthing/$host/cert; done

      # TODO check that library (nfs) is reachable
      # TODO check that circinus is working (lpstat -h crux -tv circinus somehow?)
      # TODO check that backup is running
      # TODO check that dynamic-dns is running
      # TODO check matrix bridges?
      # TODO check libvirtd / andromeda?
      # TODO check powerpanel?

      echo
      echo
      echo "✅ All systems good!"
    '';

    test = {
      lint = "${pkgs.statix}/bin/statix check .";
      dead-code = "${pkgs.deadnix}/bin/deadnix .";
      format = "${pkgs.alejandra}/bin/alejandra --check .";
    };

    fix = {
      lint = "${pkgs.statix}/bin/statix fix .";
      dead-code = "${pkgs.deadnix}/bin/deadnix -e .";
      format = "${pkgs.alejandra}/bin/alejandra .";
    };

    deploy = "${pkgs.colmena}/bin/colmena apply";

    build-iso = "${pkgs.nix}/bin/nix build -f ./isos --out-link ./iso-build";

    collect-garbage = "sudo ${pkgs.nix}/bin/nix-collect-garbage -d";

    check-vulnerabilities = "${pkgs.vulnix}/bin/vulnix --system";

    iot = pkgs.writeShellScript "iot" ''
      set -e

      jq="${pkgs.jq}/bin/jq"
      cmd="$1"
      shift

      rm -f iot-build/*.{yaml,json}
      for target in "$@"; do
        nix-build --out-link "iot-build/''${target}.json" --attr "$target" ./config/iot >/dev/null

        # Replace password commands with password values by:
        # 1) Extract the list of password commands using jq
        # 2) Send the list through bash and jq -R to actually execute the
        #    commands and format the output into json strings
        # 3) Build a subshell that outputs, first, the raw json input file, and
        #    second the list of json-stringified passwords from step 2
        # 4) Pipe the output of the subshell in step 3 into another jq command
        #    that recursively looks for password commands.  When it finds one,
        #    it replaces it with the next input.
        # This works because the order of the list generated in step 2 should
        # always match the order that jq's recursive walk finds key commands.
        # If for some reason that assumption ever fails, we'd need to change 2
        # to output something that actually keys the passwords by command to
        # reconcile them later, or switch back to python or something, but I'd
        # prefer to stay in jq for something this simple.
        (
            cat "./iot-build/''${target}.json";
            $jq -r '.. | .keyCommand? | select(. != null) | map("\"\(.)\"") | join(" ")' <"iot-build/''${target}.json" | bash | $jq -R
        ) | $jq -c 'walk(if type == "object" and .keyCommand? then input else . end)' > "iot-build/''${target}.yaml"
      done

      ${pkgs.esphome}/bin/esphome $cmd iot-build/*.yaml
    '';
  };
in
  pkgs.mkShell {
    NIX_PATH = "nixpkgs=${nixpkgs}";

    buildInputs = [
      pkgs.git
      pkgs.niv
      pkgs.passwordUtils
      pkgs.colmena
      cli
    ];
  }
