{pkgs, ...}: let
  laptopPanel = mkMonitor "BOE NE135A1M-NY1 Unknown" {
    scale = 1.5;
    width = 2880;
    height = 1920;
    refreshRate = 120;
  };

  portablePanel = mkMonitor "Audio Processing Technology  Ltd Monitor demoset-1" {
    scale = 1.4;
    width = 3840;
    height = 2160;
    refreshRate = 60;
  };

  curved = mkMonitor "LG Electronics LG ULTRAWIDE 0x0001E368" {
    scale = 1.0;
    width = 3440;
    height = 1440;
    refreshRate = 60;
  };

  left = mkMonitor "Dell Inc. DELL U3219Q H8KF413" externalMonitorDimensions;
  center = mkMonitor "Dell Inc. DELL U3219Q G3MS413" externalMonitorDimensions;
  right = mkMonitor "Dell Inc. DELL U3219Q 2ZLS413" externalMonitorDimensions;

  externalMonitorDimensions = {
    scale = 1.2;
    width = 3840;
    height = 2160;
    refreshRate = 60;
  };

  mkMonitor = id: dimensions: {
    inherit id;
    width = dimensions.width / dimensions.scale;
    height = dimensions.height / dimensions.scale;
    output = {
      criteria = id;
      status = "enable";
      inherit (dimensions) scale;
      mode = "${toString dimensions.width}x${toString dimensions.height}@${toString dimensions.refreshRate}Hz";
    };
  };

  mkPos = pos: toString (builtins.ceil pos);
in {
  primary-user.home-manager = {
    wayland.windowManager.sway.config = {
      workspaceOutputAssign = [
        {
          workspace = "1";
          output = [left.id center.id laptopPanel.id];
        }
        {
          workspace = "2";
          output = [center.id laptopPanel.id];
        }
        {
          workspace = "3";
          output = [right.id];
        }
      ];
      startup = [
        {
          command = "${pkgs.systemd}/bin/systemctl --user reload-or-restart kanshi";
          always = true;
        }
      ];
    };

    services.kanshi = {
      enable = true;
      settings = [
        {
          profile = {
            name = "laptopOnly";
            outputs = [laptopPanel.output];
          };
        }
        {
          profile = {
            name = "portablePanel";
            outputs = [
              (laptopPanel.output
                // {
                  position = "0,0";
                })

              (portablePanel.output
                // {
                  position = "${mkPos laptopPanel.width},0";
                  transform = "normal";
                })
            ];
          };
        }
        {
          profile = {
            name = "homeOfficeCenterOnly";
            outputs = [
              (laptopPanel.output
                // {
                  position = "${mkPos ((center.width - laptopPanel.width) / 2)},${mkPos center.height}";
                })

              (center.output
                // {
                  position = "0,0";
                  transform = "normal";
                })
            ];
            exec = [
              "${pkgs.sway}/bin/swaymsg \"workspace2, move workspace to '${laptopPanel.id}', focus output '${laptopPanel.id}'\", workspace 2"
              "${pkgs.sway}/bin/swaymsg \"focus output '${center.id}'\", workspace 1"
            ];
          };
        }
        {
          profile = {
            name = "homeOfficeFull";
            outputs = [
              {
                criteria = laptopPanel.id;
                status = "disable";
              }

              (left.output
                // {
                  position = "0,0";
                  transform = "270";
                })

              (center.output
                // {
                  position = "${mkPos left.height},0";
                  transform = "270";
                })

              (right.output
                // {
                  position = "${mkPos (left.height + center.height)},0";
                  transform = "270";
                })
            ];
            exec = [
              "${pkgs.sway}/bin/swaymsg \"focus output '${left.id}'\", workspace 1"
              "${pkgs.sway}/bin/swaymsg \"focus output '${right.id}'\", workspace 3"
              "${pkgs.sway}/bin/swaymsg \"focus output '${center.id}'\", workspace 2"
            ];
          };
        }
        {
          profile = {
            name = "homeOfficeCurved";
            outputs = [
              (laptopPanel.output
                // {
                  position = "${mkPos ((curved.width - laptopPanel.width) / 2)},${mkPos curved.height}";
                })

              (curved.output
                // {
                  position = "0,0";
                  transform = "normal";
                })
            ];
            exec = [
              "${pkgs.sway}/bin/swaymsg \"workspace2, move workspace to '${laptopPanel.id}', focus output '${laptopPanel.id}'\", workspace 2"
              "${pkgs.sway}/bin/swaymsg \"focus output '${curved.id}'\", workspace 1"
            ];
          };
        }
      ];
    };
  };
}
