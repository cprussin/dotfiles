{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      keepassxc = super.keepassxc.override {
        withKeePassNetworking = true;
      };
    })
  ];

  # FIXME For some reason, if keepassxc isn't added to the environment, it won't
  # start from an absolute path.  The error is:
  #
  # qt.qpa.plugin: Could not find the Qt platform plugin "xcb" in ""
  # This application failed to start because no Qt platform plugin could be
  # initialized. Reinstalling the application may fix this problem.
  #
  # This should be fixed so keepassxc can be removed from the environment and
  # accessible via the launcher only.
  home.packages = [ pkgs.keepassxc ];
}
