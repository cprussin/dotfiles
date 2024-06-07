{ pkgs, ... }: let
  network = pkgs.callPackage ./lib/network.nix {};
in pkgs.writeShellScript "systems-test" ''
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
''
