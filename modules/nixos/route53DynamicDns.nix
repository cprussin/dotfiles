{ config, lib, pkgs, ... }:
let
  cfg = config.services.route53DynamicDns;
in
{
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
        wantedBy = [ "timers.target" ];
        partOf = [ "route53-dynamic-dns-update.service" ];
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

          VALID_IP_REGEX=^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$

          currentPublicIp=$($dig +short myip.opendns.com @resolver1.opendns.com)

          if [[ ! $currentPublicIp =~ $VALID_IP_REGEX ]]; then
            echo "Could not obtain current public IP address.  Attempt yielded:"
            echo $currentPublicIp
            exit 1
          fi

          nameserver=$($aws route53 get-hosted-zone --id ZPN01N69TQ4DV | $jq -r '.DelegationSet.NameServers[0]')
          currentDnsRecord=$($dig +short ${cfg.cname} @$nameserver)

          if [[ ! $currentDnsRecord =~ $VALID_IP_REGEX ]]; then
            echo "Could not obtain current DNS record value.  Attempt yielded:"
            echo $currentDnsRecord
            exit 1
          fi

          if [[ $currentDnsRecord == $currentPublicIp ]]; then
            echo "Current IP matches DNS record value, nothing to do."
            exit
          fi

          echo "Current IP ($currentPublicIp) differs from DNS record value ($currentDnsRecord), updating!"
          read -r -d ''' BATCH <<-EOF || true
          { "Comment": "Auto-update $(date)"
          , "Changes":
              [ { "Action": "UPSERT"
                , "ResourceRecordSet":
                    { "ResourceRecords":
                      [ { "Value": "$currentPublicIp" } ]
                    , "Name": "${cfg.cname}"
                    , "Type": "A"
                    , "TTL": 60
                    }
                }
              ]
          }
          EOF
          $aws route53 change-resource-record-sets \
            --hosted-zone-id ${cfg.zoneId} \
            --change-batch file://<(echo "$BATCH")
        '';
      };
    };
  };
}
