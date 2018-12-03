{ ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      keepassxc = super.keepassxc.override {
        withKeePassNetworking = true;
      };
    })
  ];
}
