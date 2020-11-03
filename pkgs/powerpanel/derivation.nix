{ stdenv, autoPatchelfHook }:
let
  pname = "powerpanel";
  version = "1.3.3";
  unpacked = "${pname}-${version}";
in
stdenv.mkDerivation {
  inherit pname version;

  src = fetchTarball {
    url = "https://dl4jz3rbrsfum.cloudfront.net/software/PPL-${version}-64bit.tar.gz";
    sha256 = "1xw0b5q5kwqrk3pbbxg9n6lrargvhs4xbjxznxqiw0gi73lidaav";
  };

  nativeBuildInputs = [ autoPatchelfHook ];

  installPhase = ''
    install -m755 -D -t $out/bin ${unpacked}/bin/*
    install -m644 -D -t $out/share/man/man8 ${unpacked}/doc/{pwrstat.8,pwrstatd.8}
    install -m644 -D -t $out/share/doc ${unpacked}/doc/{LICENSE,README,deploy-guide,install-guide,user-manual}
  '';
}
