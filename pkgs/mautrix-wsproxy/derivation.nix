{ src, buildGoModule }:

buildGoModule {
  inherit src;
  pname = "mautrix-wsproxy";
  version = "1.0.0";
  vendorSha256 = "0h8zqlzigdnixd48gnqfyfg2jrsxzg7p013aylsh5myvjvjjdh9h";
}
