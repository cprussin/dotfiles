{
  trivialBuild,
  src,
}:
trivialBuild {
  inherit src;
  version = "master";
  pname = "zoom-frm";
}
