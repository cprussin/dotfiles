_: {
  primary-user.home-manager.services.kanshi = {
    enable = true;
    settings = [
      {
        profile = {
          name = "laptopOnly";
          outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
            }
          ];
        };
      }
      {
        profile = {
          name = "homeOffice";
          outputs = [
            {
              criteria = "eDP-1";
              status = "disable";
            }

            {
              criteria = "Dell Inc. DELL U3219Q H8KF413";
              status = "enable";
              transform = "270";
              position = "0,0";
            }

            {
              criteria = "Dell Inc. DELL U3219Q G3MS413";
              status = "enable";
              transform = "270";
              position = "2160,0";
            }

            {
              criteria = "Dell Inc. DELL U3219Q 2ZLS413";
              status = "enable";
              transform = "270";
              position = "4320,0";
            }
          ];
        };
      }
    ];
  };
}
