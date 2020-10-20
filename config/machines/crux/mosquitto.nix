{ config, lib, pkgs, ... }:

let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};

  tasmotaDevices = [
    "left-turtle-tank-lamp"
    "right-turtle-tank-lamp"
  ];

  tasmotaUsers = lib.listToAttrs (
    map (
      device: lib.nameValuePair device {
        hashedPassword = passwords.get-hashed-mosquitto-password device;
        acl = [
          "topic read cmnd/${device}/#"
          "topic write stat/${device}/#"
          "topic write tele/${device}/#"
          "topic write homeassistant/#"
        ];
      }
    ) tasmotaDevices
  );

  cafile = pkgs.writeText "mosquitto-ca" ''
    -----BEGIN CERTIFICATE-----
    MIID8TCCAtmgAwIBAgIUMUBOEKGdXzqbetIDPsWmfkeyCbYwDQYJKoZIhvcNAQEL
    BQAwgYcxCzAJBgNVBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMREwDwYDVQQH
    DAhTYW4gSm9zZTETMBEGA1UECgwKUHJ1c3Npbk5ldDEZMBcGA1UECwwQNzIwIE5h
    dG9tYSBEcml2ZTEgMB4GCSqGSIb3DQEJARYRYWRtaW5AcHJ1c3Npbi5uZXQwHhcN
    MjAxMDIwMDkwMzQ4WhcNMjUxMDIwMDkwMzQ4WjCBhzELMAkGA1UEBhMCVVMxEzAR
    BgNVBAgMCkNhbGlmb3JuaWExETAPBgNVBAcMCFNhbiBKb3NlMRMwEQYDVQQKDApQ
    cnVzc2luTmV0MRkwFwYDVQQLDBA3MjAgTmF0b21hIERyaXZlMSAwHgYJKoZIhvcN
    AQkBFhFhZG1pbkBwcnVzc2luLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCC
    AQoCggEBAN9PPkQlxsRJiLxf4isYw6jUqeCkZtOkYOKa+nYlbV5bFGExVLESGNJ/
    aRzr0Cm6tYLmdneuwShTBmNtwX+aElhivH0i3b0AGnJo728kYisFH6UdWUQXqmOz
    s6pedmmGtOccF/et431kiYKeZeeEBQrCkr8HtQWdemgx4RItBAaOBCPU2LXQ23Qy
    IWWCEs+tZkQSlUUbr+7t7h/kYot1ELs8IYf2l1HkeDrZBbDXfaQ/iti3FrFrkoeJ
    ljl+jCiRo60r5b629H7k2tLgUZAxvM3WAtVn7Tmj5BdJP4DpC7V3tw+Wzu65QwwD
    ZNoiw6QpG8SZpZcEeWw5hvNkx9a5u6kCAwEAAaNTMFEwHQYDVR0OBBYEFG3uQZ9Z
    Gw5RSq79XJRj3FfjCJFAMB8GA1UdIwQYMBaAFG3uQZ9ZGw5RSq79XJRj3FfjCJFA
    MA8GA1UdEwEB/wQFMAMBAf8wDQYJKoZIhvcNAQELBQADggEBAIHOK0xBPMT+KOE+
    cWyulRK6twWWAORjDYXWy4klLnf3+GhTKjXsdZGnaNgTMnImV640hq29xy1AEB2z
    ClueFjw+UMwJyGxg/h5N2ZBaC04dkqg/mBg7b/4ftxSoMdQDH4yV3xqbk9sg+bex
    W6W0aJhGtOB9CC81tFjvdZCEFP4t1qN65PokGU3N55Wsb/7np5hQfWa5MXtn18RT
    dxcV25UH2+INt0NELty2vJq86tNoz4uNPTYZp/0R5t3DUKdEida/LhuEGtggTeXt
    BTKfdUkSP4p1PSh+ERXdpHlgitvaHd6dm6jqqcDOOrkJtxfEagDh5eEC7R9ZV3iC
    iEKFugY=
    -----END CERTIFICATE-----
  '';

  certfile = pkgs.writeText "mosquitto-cert" ''
    -----BEGIN CERTIFICATE-----
    MIIDpjCCAo4CFFP9PppBYEoJWhFXJexCcuw4hUXnMA0GCSqGSIb3DQEBCwUAMIGH
    MQswCQYDVQQGEwJVUzETMBEGA1UECAwKQ2FsaWZvcm5pYTERMA8GA1UEBwwIU2Fu
    IEpvc2UxEzARBgNVBAoMClBydXNzaW5OZXQxGTAXBgNVBAsMEDcyMCBOYXRvbWEg
    RHJpdmUxIDAeBgkqhkiG9w0BCQEWEWFkbWluQHBydXNzaW4ubmV0MB4XDTIwMTAy
    MDA5MDYwMVoXDTIxMTAxNTA5MDYwMVowgZYxCzAJBgNVBAYTAlVTMRMwEQYDVQQI
    DApDYWxpZm9ybmlhMREwDwYDVQQHDAhTYW4gSm9zZTETMBEGA1UECgwKUHJ1c3Np
    bk5ldDEZMBcGA1UECwwQNzIwIE5hdG9tYSBEcml2ZTENMAsGA1UEAwwEY3J1eDEg
    MB4GCSqGSIb3DQEJARYRYWRtaW5AcHJ1c3Npbi5uZXQwggEiMA0GCSqGSIb3DQEB
    AQUAA4IBDwAwggEKAoIBAQCkevhKqQ1OERzRH+5aVVxU8flAoraWYYP/zlcL35NP
    LbKXO2Ttd3ptyCik89RqososIw9P/OZdA6j7PdKoj7T6qEkfx8ivXQ4uraXZZKcF
    ENGKwuXlgkb7dt7NI1JOrj8czfTJ9+DrsDNLwWqcOE518Kwb1jX9zgW16ohuWnX2
    uiI5BIsoxXo+2OZ+Zov2WLkPa433/s9fvl0YtvnS90AZ1Tiv/Wl3HXi7X6Ox4Lle
    R6anmKPHUlLzuxN1ZOuaWQjWdcgGs24QV0DDFssa/rlzMCFpeNAdrv5RtWMTGCnz
    oOYOtDZP+FSlJw16oxWJlrRY2vfGUrcIgNVI24aj2nwxAgMBAAEwDQYJKoZIhvcN
    AQELBQADggEBAJpwOdy3rLhZqaZ7hqhB66f1SCltXIrQHai+AhTogLku9AbjY0uY
    CMPdJScwTJ7SqlH+2lnanyxZoEvg4Dkx45y6Vl0KdRKsrwzEKAe2goaBjKcMShUB
    QayZtyo7SdrV/8hdde8qUYRXeerT4xRGek6IhUXV7lnf7x0xaeUtwaVddzR3gaJr
    9anSr/YimJlZuEw4WUY7ICG8zWjeODX9tiXwyn9G5CkzMpA6D946jK1rQ8LZ5G6X
    WfSXWNsf5jNL7YvzYMGVYEZs9zHPigNbuY0O0k4TxXhbY5Wx6/6FTSF8xY6hZVoY
    sDKyHxbHsicEdaNxTqfNKVkUWIF6Rnn4oas=
    -----END CERTIFICATE-----
  '';
in

{
  networking.firewall.allowedTCPPorts = [ config.services.mosquitto.ssl.port ];

  deployment.keys.mosquitto-ssl-key = {
    text = passwords.get-full-password "Infrastructure/IoT/mqtt/mosquitto-private-ssl-key";
    user = "mosquitto";
    group = "mosquitto";
  };

  services.mosquitto = {
    enable = true;
    ssl = {
      inherit cafile certfile;

      enable = true;
      keyfile = config.deployment.keys.mosquitto-ssl-key.path;
    };
    users = tasmotaUsers // {
      home-assistant = {
        hashedPassword = passwords.get-hashed-mosquitto-password "home-assistant";
        acl =
          lib.foldl
            (
              list: device:
                list ++ [
                  "topic write cmnd/${device}/#"
                  "topic read stat/${device}/#"
                  "topic read tele/${device}/#"
                ]
            )
            [ "topic read homeassistant/#" ]
            tasmotaDevices;
      };
    };
    checkPasswords = true;
  };

  systemd.services.mosquitto = {
    after = [ "mosquitto-ssl-key-key.service" ];
    wants = [ "mosquitto-ssl-key-key.service" ];
  };

  users.users.mosquitto.extraGroups = [ "keys" ];
}
