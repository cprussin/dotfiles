{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.programs.powerpanel;
  yesNo = bool:
    if bool
    then "yes"
    else "no";
in {
  options.programs.powerpanel = {
    enable = lib.mkEnableOption "PowerPanel";

    powerfail = {
      delay = lib.mkOption {
        description = ''
          A delay time in seconds since event of Power Failure occur then to run
          shell script and shutdown system.
        '';
        type = lib.types.ints.between 0 3600;
        default = 60;
      };

      active = lib.mkOption {
        description = ''
          Enable to run shell script when the event of Power Failure occur.
        '';
        type = lib.types.bool;
        default = false;
      };

      cmd-path = lib.mkOption {
        description = ''
          Assign a path of script file for event of Power Failure.
        '';
        type = lib.types.nullOr lib.types.path;
        default = null;
      };

      duration = lib.mkOption {
        description = ''
          How much time in seconds to take script running for event of Power
          Failure.
        '';
        type = lib.types.ints.between 0 3600;
        default = 0;
      };

      shutdown = lib.mkOption {
        description = ''
          Allow Daemon to shutdown system for event of Power Failure.
        '';
        type = lib.types.bool;
        default = false;
      };
    };

    lowbatt = {
      threshold = lib.mkOption {
        description = ''
          A threshold of Battery Capacity, If the battery capacity is lower than
          this value and a event of Battery Low will be identified. The unit is
          percentage.
        '';
        type = lib.types.ints.between 0 90;
        default = 35;
      };

      runtime-threshold = lib.mkOption {
        description = ''
          A threshold of Remaining Runtime, If the Remaining Runtime is lower
          than this value and a event of Battery Low will be identified. The
          unit is second. Note: When meet this condition the below
          'shutdown-sustain' property will be ignored.
        '';
        type = lib.types.ints.between 0 3600;
        default = 300;
      };

      active = lib.mkOption {
        description = ''
          Enable to run shell script when the event of Battery Low occur.
        '';
        type = lib.types.bool;
        default = false;
      };

      cmd-path = lib.mkOption {
        description = "Assign a path of script file for event of Battery Low.";
        type = lib.types.nullOr lib.types.path;
        default = null;
      };

      duration = lib.mkOption {
        description = ''
          How much time in seconds to take script running for event of Battery
          Low.
        '';
        type = lib.types.ints.between 0 60;
        default = 0;
      };

      shutdown = lib.mkOption {
        description = ''
          Allow Daemon to shutdown system for event of Battery Low.
        '';
        type = lib.types.bool;
        default = false;
      };
    };

    enable-alarm = lib.mkOption {
      description = "Turn UPS alarm on or off.";
      type = lib.types.bool;
      default = true;
    };

    shutdown-sustain = lib.mkOption {
      description = ''
        The necessary time in seconds for system shutdown. The UPS will turn
        power off when this time is expired. If the computer shutdown is cause
        by low runtime condition, the UPS will turn power off when the time is
        expired that time is assigned on 'runtime-threshold' property and it is
        no longer to refer the 'shutdown-sustain' property.
      '';
      type = lib.types.ints.between 0 3600;
      default = 600;
    };

    turn-ups-off = lib.mkOption {
      description = ''
        Daemon will turn UPS power off once it ask system shutdown cause by a
        power event.
      '';
      type = lib.types.bool;
      default = false;
    };

    ups-polling-rate = lib.mkOption {
      description = "The period of polling UPS in seconds.";
      type = lib.types.ints.between 1 60;
      default = 3;
    };

    #  The allowed range is 1 ~ 300. Default is 10 sec.
    ups-retry-rate = lib.mkOption {
      description = ''
        The period of re-try to find available UPS in seconds since find nothing
        at last time.
      '';
      type = lib.types.ints.between 1 300;
      default = 10;
    };

    #  Allowed options are yes and no. Default is no.
    prohibit-client-access = lib.mkOption {
      description = ''
        Prohibiting daemon to provide communication mechanism for client, such
        as pwrstat command. normally, it should be `false`. It can be `true` if
        any security consideration.
      '';
      type = lib.types.bool;
      default = false;
    };

    allowed-device-nodes = lib.mkOption {
      description = ''
        The pwrstatd accepts four types of device node which includes the
        'ttyS', 'ttyUSB', 'hiddev', and 'libusb' for communication with UPS. The
        pwrstatd defaults to enumerate all acceptable device nodes and pick up
        to use an available device node automatically. But this may cause a
        disturbance to the device node which is occupied by other
        software. Therefore, you can restrict this enumerate behave by using
        allowed-device-nodes option. You can assign the single device node path
        or multiple device node paths divided by a semicolon at this option. All
        groups of 'ttyS', 'ttyUSB', 'hiddev', or 'libusb' device node are
        enumerated without a suffix number assignment.  Note, the 'libusb' does
        not support suffix number only.

        For example: restrict to use ttyS1, ttyS2 and hiddev1 device nodes at
        /dev path only.
        allowed-device-nodes = /dev/ttyS1;/dev/ttyS2;/dev/hiddev1

        For example: restrict to use ttyS and ttyUSB two groups of device node
        at /dev,/dev/usb, and /dev/usb/hid paths(includes ttyS0 to ttySN and
        ttyUSB0 to ttyUSBN, N is number).
        allowed-device-nodes = ttyS;ttyUSB

        For example: restrict to use hiddev group of device node at
        /dev,/dev/usb, and /dev/usb/hid paths(includes hiddev0 to hiddevN, N is
        number).
        allowed-device-nodes = hiddev

        For example: restrict to use libusb device.
        allowed-device-nodes = libusb
      '';
      type = lib.types.str;
      default = "";
    };

    hibernate = lib.mkOption {
      description = ''
        Daemon will hibernate system to instead of system shutdown when power
        event occur.
      '';
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.pwrstatd = {
      description = "CyberPower UPS monitoring service.";
      wantedBy = ["multi-user.target"];
      serviceConfig.ExecStart = "${pkgs.powerpanel}/bin/pwrstatd";
    };

    environment = {
      systemPackages = [pkgs.powerpanel];

      etc."pwrstatd.conf".text = ''
        powerfail-delay = ${toString cfg.powerfail.delay}
        powerfail-active = ${yesNo cfg.powerfail.active}
        powerfail-cmd-path = ${
          if cfg.powerfail.cmd-path == null
          then ""
          else cfg.powerfail.cmd-path
        }
        powerfail-duration = ${toString cfg.powerfail.duration}
        powerfail-shutdown = ${yesNo cfg.powerfail.shutdown}
        lowbatt-threshold = ${toString cfg.lowbatt.threshold}
        runtime-threshold = ${toString cfg.lowbatt.runtime-threshold}
        lowbatt-active = ${yesNo cfg.lowbatt.active}
        lowbatt-cmd-path = ${
          if cfg.lowbatt.cmd-path == null
          then ""
          else cfg.lowbatt.cmd-path
        }
        lowbatt-duration = = ${toString cfg.lowbatt.duration}
        lowbatt-shutdown =  ${yesNo cfg.lowbatt.shutdown}
        enable-alarm = ${yesNo cfg.enable-alarm}
        shutdown-sustain = ${toString cfg.shutdown-sustain}
        turn-ups-off = ${yesNo cfg.turn-ups-off}
        ups-polling-rate = ${toString cfg.ups-polling-rate}
        ups-retry-rate = ${toString cfg.ups-retry-rate}
        prohibit-client-access = ${yesNo cfg.prohibit-client-access}
        allowed-device-nodes = ${cfg.allowed-device-nodes}
        hibernate = ${yesNo cfg.hibernate}
      '';
    };
  };
}
