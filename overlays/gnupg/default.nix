self: super: {
  gnupg = self.callPackage ../../pkgs/gnupg {
    gnupg = super.gnupg;
  };
}
