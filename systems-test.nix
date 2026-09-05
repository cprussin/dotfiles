{pkgs, ...}: let
  inherit (pkgs) lib;

  network = pkgs.callPackage ./lib/network.nix {};

  # Internal hosts that serve https with a cert from the password store.
  tlsHosts = [
    "library.internal.prussin.net"
    "home-assistant.internal.prussin.net"
    "matrix.internal.prussin.net"
    "passwords.internal.prussin.net"
    "eyes.internal.prussin.net"
    "photos.internal.prussin.net"
    "private.internal.prussin.net"
  ];
in
  pkgs.writeShellScript "systems-test" ''
    pass="${pkgs.pass}/bin/pass"
    curl="${pkgs.curl}/bin/curl"
    jq="${pkgs.jq}/bin/jq"
    dig="${pkgs.dnsutils}/bin/dig"
    lpstat="${pkgs.cups}/bin/lpstat"
    openssl="${pkgs.openssl}/bin/openssl"
    ssh="${pkgs.openssh}/bin/ssh"
    timeout="${pkgs.coreutils}/bin/timeout"
    bash="${pkgs.bash}/bin/bash"
    xmllint="${pkgs.libxml2}/bin/xmllint"

    if [ -t 1 ]; then
        bold=$'\e[1m'
        dim=$'\e[2m'
        red=$'\e[31m'
        green=$'\e[32m'
        reset=$'\e[0m'
    else
        bold=""
        dim=""
        red=""
        green=""
        reset=""
    fi

    # Column that check statuses are padded out to, so they line up.
    STATUS_COLUMN=72

    TOTAL=0
    FAILURES=()

    # Run a check, printing its output only when it fails.  A check is any
    # command that exits nonzero and explains itself on stdout or stderr when
    # whatever it's testing is broken.
    check() {
        local description="$1"
        shift
        TOTAL=$((TOTAL + 1))

        # Pad the description out with dots so that the status column lines up
        # and a failure is easy to spot while scanning down the output.
        local label="Checking $description "
        local width=$((STATUS_COLUMN - ''${#label}))
        local dots=""
        if [ "$width" -gt 0 ]; then
            dots=$(printf "%''${width}s" "" | tr " " ".")
        fi
        printf "%s%s%s%s " "$label" "$dim" "$dots" "$reset"

        local output
        if output=$("$@" 2>&1); then
            printf "%sok%s\n" "$green" "$reset"
        else
            printf "%s%sFAILED%s\n" "$bold" "$red" "$reset"
            FAILURES+=("$description")
            # Indent the failure behind a rule so it reads as belonging to the
            # check above it rather than to the checks that follow.
            local line
            while IFS= read -r line; do
                printf "  %s│%s %s\n" "$red" "$reset" "$line"
            done <<<"''${output:-(the check failed without explaining itself)}"
        fi
    }

    fetch() {
        $curl -s --max-time 10 "$@"
    }

    # Fetch a url and compare the response body against an expected value.
    expect_body() {
        local url="$1"
        local expected="$2"
        local body
        if ! body=$(fetch "$url"); then
            echo "Could not connect to $url!"
            return 1
        elif [ "$body" != "$expected" ]; then
            echo "Unexpected response from $url: $body"
            return 1
        fi
    }

    # Fetch a url and compare the response status against an expected value.
    expect_status() {
        local url="$1"
        shift
        local status
        if ! status=$(fetch -o /dev/null -w '%{http_code}' "$url"); then
            echo "Could not connect to $url!"
            return 1
        fi
        local expected
        for expected in "$@"; do
            if [ "$status" == "$expected" ]; then
                return 0
            fi
        done
        echo "$url returned http $status!"
        return 1
    }

    ping_host() {
        if ! ping -q -c 1 -W 1 "$1" >/dev/null 2>&1; then
            echo "$2 is down!"
            return 1
        fi
    }

    on_crux() {
        $ssh -o BatchMode=yes -o ConnectTimeout=10 root@crux "$@"
    }

    # Check that a systemd unit on crux is running.
    expect_active_unit() {
        local unit="$1"
        local state
        state=$(on_crux "systemctl is-active $unit" 2>&1)
        if [ "$state" != "active" ]; then
            echo "$unit on crux is $state!"
            return 1
        fi
    }

    # Check that a periodically scheduled job on crux is both scheduled and
    # succeeding.
    expect_healthy_timer() {
        local unit="$1"
        local res
        if ! res=$(on_crux "systemctl is-active $unit.timer; systemctl show -p Result --value $unit.service" 2>&1); then
            echo "Could not query $unit on crux: $res"
            return 1
        fi
        local timer result
        timer=$(echo "$res" | sed -n 1p)
        result=$(echo "$res" | sed -n 2p)
        if [ "$timer" != "active" ]; then
            echo "The $unit timer is $timer!"
            return 1
        elif [ "$result" != "success" ]; then
            echo "The last $unit run finished with result \"$result\"!"
            return 1
        fi
    }

    check "crux is up on wireguard ipv6" \
        ping_host ${network.wireguard6.crux.address} "Crux (ipv6)"

    check "crux is up on wireguard ipv4" \
        ping_host ${network.wireguard4.crux.address} "Crux (ipv4)"

    check_internal_dns() {
        local res
        res=$($dig +short AAAA crux.internal.prussin.net)
        if [ "$res" != "${network.wireguard6.crux.address}" ]; then
            echo "Expected crux.internal.prussin.net to resolve to ${network.wireguard6.crux.address}, got: $res"
            return 1
        fi
    }
    check "internal dns is up" check_internal_dns

    check_forwarded_dns() {
        local res
        res=$($dig +short A one.one.one.one)
        if ! echo "$res" | grep -qE '^[0-9]+(\.[0-9]+){3}$'; then
            echo "Could not resolve an external name through crux, got: $res"
            return 1
        fi
    }
    check "dns forwarding is working" check_forwarded_dns

    check_dynamic_dns() {
        expect_healthy_timer cloudflare-dyndns || return 1
        local a aaaa
        a=$($dig +short A crux.prussin.net)
        aaaa=$($dig +short AAAA crux.prussin.net)
        if ! echo "$a" | grep -qE '^[0-9]+(\.[0-9]+){3}$'; then
            echo "crux.prussin.net has no valid A record, got: $a"
            return 1
        elif ! echo "$aaaa" | grep -q ':'; then
            echo "crux.prussin.net has no valid AAAA record, got: $aaaa"
            return 1
        fi
    }
    check "dynamic dns is running" check_dynamic_dns

    check_library_http() {
        local body
        if ! body=$(fetch https://library.internal.prussin.net); then
            echo "Could not connect to library (http)!"
            return 1
        fi
        local title
        title=$(echo "$body" | $xmllint --html --xpath 'string(/html/head/title)' - 2>/dev/null)
        if [ "$title" != "Index of /" ]; then
            echo "Unexpected page served by library (http): $title"
            return 1
        fi
    }
    check "library (http) is running" check_library_http

    check_library_ftp() {
        if ! fetch ftp://library.internal.prussin.net >/dev/null; then
            echo "Library (ftp) is down!"
            return 1
        fi
    }
    check "library (ftp) is running" check_library_ftp

    # Only nfsv4 (port 2049) is exposed, so the mount protocol that showmount
    # uses isn't reachable and all we can do without root is check that the
    # server accepts connections.
    check_library_nfs() {
        if ! $timeout 5 $bash -c 'echo > /dev/tcp/library.internal.prussin.net/2049' 2>/dev/null; then
            echo "Library (nfs) is not accepting connections on port 2049!"
            return 1
        fi
    }
    check "library (nfs) is reachable" check_library_nfs

    check "matrix (client) is running" \
        expect_body https://matrix.internal.prussin.net/health OK

    check "matrix (federation) is running" \
        expect_body https://matrix.prussin.net:8448/health OK

    check "immich is running" \
        expect_body https://photos.internal.prussin.net/api/server/ping '{"res":"pong"}'

    check_vaultwarden() {
        local body
        if ! body=$(fetch https://passwords.internal.prussin.net/api/alive); then
            echo "Could not connect to vaultwarden!"
            return 1
        elif [[ ! "$body" =~ $(date -uI) ]]; then
            echo "Unexpected response from vaultwarden: $body"
            return 1
        fi
    }
    check "vaultwarden is running" check_vaultwarden

    check_home_assistant() {
        local key body
        key="$($pass show "Connor/Home/Home Assistant" 2>/dev/null | grep "^API Key: " | sed "s/^API Key: //")"
        if [ -z "$key" ]; then
            echo "Could not read the home assistant api key from the password store!"
            return 1
        fi
        body=$(fetch -H "Authorization: Bearer $key" -H "Content-Type: application/json" https://home-assistant.internal.prussin.net/api/)
        if [ "$(echo "$body" | $jq -r '.message' 2>/dev/null)" != "API running." ]; then
            echo "Unexpected response from home assistant: $body"
            return 1
        fi
    }
    check "home assistant is running" check_home_assistant

    check "zwave-js is running" expect_active_unit zwave-js

    # Frigate requires authentication, so an unauthenticated request gets a 401
    # from the backend when it's healthy.  If the backend is down nginx can't
    # run its auth subrequest and returns a 5xx instead.
    check "eyes is running" \
        expect_status https://eyes.internal.prussin.net/api/version 200 401

    # The stats api is behind frigate's auth, so read it from crux rather than
    # over the public vhost.  Frigate rejects requests that didn't come through
    # nginx's auth subrequest, so the api port (5001) answers 401 -- nginx
    # listens on 127.0.0.1:5000 for exactly this, and frigate treats requests
    # arriving on that port as an authenticated admin.
    check_cameras() {
        local stats down
        if ! stats=$(on_crux 'curl -sS --fail-with-body --max-time 10 http://127.0.0.1:5000/api/stats' 2>&1); then
            echo "Could not read frigate stats from crux: $stats"
            return 1
        fi
        if ! down=$(echo "$stats" | $jq -er '.cameras | to_entries | map(select(.value.camera_fps == 0)) | map(.key) | join(", ")' 2>&1); then
            echo "Could not parse frigate stats: $stats"
            return 1
        elif [ -n "$down" ]; then
            echo "Cameras not capturing: $down"
            return 1
        fi
    }
    check "all cameras are capturing" check_cameras

    check "private is running" expect_status https://private.internal.prussin.net 200

    check_circinus() {
        local res
        if ! res=$($lpstat -h circinus.internal.prussin.net -p circinus 2>&1); then
            echo "Could not query cups on crux: $res"
            return 1
        elif ! echo "$res" | grep -q "enabled since"; then
            echo "Circinus is not enabled: $res"
            return 1
        fi
    }
    check "circinus is running" check_circinus

    syncthing_api() {
        local config="''${XDG_CONFIG_HOME:-$HOME/.config}/syncthing/config.xml"
        if [ ! -f "$config" ]; then
            echo "There is no syncthing config at $config!"
            return 1
        fi
        local key address
        key=$($xmllint --xpath 'string(/configuration/gui/apikey)' "$config")
        address=$($xmllint --xpath 'string(/configuration/gui/address)' "$config")
        $curl -sf --max-time 10 -H "X-API-Key: $key" "http://$address/rest/$1"
    }

    check_syncthing() {
        local status devices connections disconnected
        if ! status=$(syncthing_api system/status 2>&1); then
            echo "Could not query the local syncthing instance: $status"
            return 1
        elif ! devices=$(syncthing_api config/devices 2>&1); then
            echo "Could not read the syncthing device list: $devices"
            return 1
        elif ! connections=$(syncthing_api system/connections 2>&1); then
            echo "Could not read the syncthing connection list: $connections"
            return 1
        fi
        if ! disconnected=$(
            echo "$devices" | $jq -er \
                --arg myId "$(echo "$status" | $jq -r '.myID')" \
                --argjson connections "$connections" \
                'map(
                    select(.deviceID != $myId)
                    | select($connections.connections[.deviceID].connected != true)
                    | .name
                 )
                 | join(", ")' 2>&1
        ); then
            echo "Could not read syncthing connections: $disconnected"
            return 1
        elif [ -n "$disconnected" ]; then
            echo "Syncthing is not connected to: $disconnected"
            return 1
        fi
    }
    check "syncthing connections" check_syncthing

    check "backup is running" expect_healthy_timer borgbackup-job-rsync.net

    check_pools() {
        local res
        if ! res=$(on_crux 'zpool status -x' 2>&1); then
            echo "Could not query zpool status on crux: $res"
            return 1
        elif [ "$res" != "all pools are healthy" ]; then
            echo "$res"
            return 1
        fi
    }
    check "crux's zpools are healthy" check_pools

    # Certs for internal hosts are self-signed and rotated by hand, so warn well
    # before they expire.
    check_cert() {
        local host="$1"
        local cert
        if ! cert=$(echo | $timeout 10 $openssl s_client -connect "$host:443" -servername "$host" 2>/dev/null); then
            echo "Could not fetch the certificate for $host!"
            return 1
        elif ! echo "$cert" | $openssl x509 -checkend $((30 * 24 * 60 * 60)) >/dev/null 2>&1; then
            echo "The certificate for $host expires $(echo "$cert" | $openssl x509 -noout -enddate | sed 's/^notAfter=//')!"
            return 1
        fi
    }

    for host in ${lib.concatStringsSep " " tlsHosts}; do
        check "the certificate for $host" check_cert "$host"
    done

    # TODO check powerpanel?

    echo
    if [ ''${#FAILURES[@]} -eq 0 ]; then
        printf "%s%sAll %d checks passed!%s ✅\n" "$bold" "$green" "$TOTAL" "$reset"
    else
        printf "%s%s%d of %d checks failed!%s ❌\n" \
            "$bold" "$red" "''${#FAILURES[@]}" "$TOTAL" "$reset"
        for failure in "''${FAILURES[@]}"; do
            printf "  %s✘%s %s\n" "$red" "$reset" "$failure"
        done
        exit 1
    fi
  ''
