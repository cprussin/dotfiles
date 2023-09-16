{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.route53DynamicDns;
in {
  options.services.route53DynamicDns = {
    enable = lib.mkEnableOption "Route 53 Dynamic DNS";

    zoneId = lib.mkOption {
      type = lib.types.str;
    };

    cname = lib.mkOption {
      type = lib.types.str;
    };

    accessKeyFile = lib.mkOption {
      type = lib.types.path;
    };

    secretAccessKeyFile = lib.mkOption {
      type = lib.types.path;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd = {
      timers.route53-dynamic-dns-update = {
        wantedBy = ["timers.target"];
        partOf = ["route53-dynamic-dns-update.service"];
        timerConfig.OnCalendar = "minutely";
      };

      services.route53-dynamic-dns-update = {
        description = "Update Route53 with current public IP.";
        serviceConfig.Type = "oneshot";
        script = ''
          set -e

          aws=${pkgs.awscli}/bin/aws
          dig=${pkgs.dnsutils}/bin/dig
          jq=${pkgs.jq}/bin/jq

          export AWS_ACCESS_KEY_ID=$(<${cfg.accessKeyFile})
          export AWS_SECRET_ACCESS_KEY=$(<${cfg.secretAccessKeyFile})

          VALID_IPV4_REGEX=^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$
          VALID_IPV6_REGEX="^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$"

          currentPublicIpv4=$($dig +short myip.opendns.com @resolver1.opendns.com)

          if [[ ! $currentPublicIpv4 =~ $VALID_IPV4_REGEX ]]; then
            currentPublicIpv4=$($dig -4 TXT +short o-o.myaddr.l.google.com @ns1.google.com | sed 's/"//g')
          fi

          if [[ ! $currentPublicIpv4 =~ $VALID_IPV4_REGEX ]]; then
            echo "Could not obtain current public IPv4 address.  Attempt yielded:"
            echo $currentPublicIpv4
            exit 1
          fi

          currentPublicIpv6=$($dig -6 TXT +short o-o.myaddr.l.google.com @ns1.google.com | sed 's/"//g')

          if [[ ! $currentPublicIpv6 =~ $VALID_IPV6_REGEX ]]; then
            echo "Could not obtain current public IPv6 address.  Attempt yielded:"
            echo $currentPublicIpv6
            exit 1
          fi

          nameserver=$($aws route53 get-hosted-zone --id ${cfg.zoneId} | $jq -r '.DelegationSet.NameServers[0]')
          currentARecord=$($dig +short A ${cfg.cname} @$nameserver)
          currentAAAARecord=$($dig +short AAAA ${cfg.cname} @$nameserver)

          if [[ ! $currentARecord =~ $VALID_IPV4_REGEX ]]; then
            echo "Could not obtain current DNS A record value.  Attempt yielded:"
            echo $currentARecord
            exit 1
          fi

          if [[ ! $currentAAAARecord =~ $VALID_IPV6_REGEX ]]; then
            echo "Could not obtain current DNS AAAA record value.  Attempt yielded:"
            echo $currentAAAARecord
            exit 1
          fi

          changes=()

          if [[ $currentARecord == $currentPublicIpv4 ]]; then
            echo "Current IPv4 matches DNS record value."
          else
            echo "Current IPv4 ($currentPublicIpv4) differs from DNS record value ($currentARecord), updating!"
            read -r -d ''' change <<-EOF || true
              { "Action": "UPSERT"
              , "ResourceRecordSet":
                { "ResourceRecords": [ { "Value": "$currentPublicIpv4" } ]
                , "Name": "${cfg.cname}"
                , "Type": "A"
                , "TTL": 3600
                }
              }
          EOF
            changes+=( "$change" )
          fi

          if [[ $currentAAAARecord == $currentPublicIpv6 ]]; then
            echo "Current IPv6 matches DNS record value."
          else
            echo "Current IPv6 ($currentPublicIpv6) differs from DNS record value ($currentAAAARecord), updating!"
            read -r -d ''' change <<-EOF || true
              { "Action": "UPSERT"
              , "ResourceRecordSet":
                { "ResourceRecords": [ { "Value": "$currentPublicIpv6" } ]
                , "Name": "${cfg.cname}"
                , "Type": "AAAA"
                , "TTL": 3600
                }
              }
          EOF
            changes+=( "$change" )
          fi

          if [[ ''${#changes[@]} == 0 ]]; then
            echo "Nothing to do!"
            exit
          elif [[ ''${#changes[@]} == 1 ]]; then
            read -r -d ''' BATCH <<-EOF || true
              { "Comment": "Auto-update $(date)"
              , "Changes": [ ''${changes[0]} ]
              }
          EOF
          else
            read -r -d ''' BATCH <<-EOF || true
              { "Comment": "Auto-update $(date)"
              , "Changes": [ ''${changes[0]}, ''${changes[1]} ]
              }
          EOF
          fi

          $aws route53 change-resource-record-sets \
            --hosted-zone-id ${cfg.zoneId} \
            --change-batch file://<(echo "$BATCH")
        '';
      };
    };
  };
}
