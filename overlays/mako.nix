self: super: {
  mako = self.callPackage ../pkgs/mako {
    mako = super.mako;
  };
}
