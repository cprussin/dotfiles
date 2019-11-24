self: super: {
  slack = self.callPackage ../pkgs/slack {
    slack = super.slack;
  };
}
