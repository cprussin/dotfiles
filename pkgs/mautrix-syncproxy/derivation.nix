{
  src,
  buildGoModule,
}:
buildGoModule {
  inherit src;
  pname = "mautrix-syncproxy";
  version = "master";
  vendorSha256 = "0wg6m0qml6sgiaf0s2mv7m4kk95ajyl79kxasrql15p33slij7mz";
}
