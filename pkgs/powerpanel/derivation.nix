{
  stdenv,
  autoPatchelfHook,
}: let
  pname = "powerpanel";
  version = "1.3.3";
in
  stdenv.mkDerivation {
    inherit pname version;

    src = builtins.fetchurl {
      url = "https://dl4jz3rbrsfum.cloudfront.net/software/PPL-${version}-64bit.tar.gz";
      sha256 = "0isr1a5lp46x56p8ijhlz309zp2k52i95aajq5xvk57j2lxhxanx";
    };

    nativeBuildInputs = [autoPatchelfHook];

    installPhase = ''
      install -m755 -D -t $out/bin bin/*
      install -m644 -D -t $out/share/man/man8 doc/{pwrstat.8,pwrstatd.8}
      install -m644 -D -t $out/share/doc doc/{LICENSE,README,deploy-guide,install-guide,user-manual}
    '';
  }
