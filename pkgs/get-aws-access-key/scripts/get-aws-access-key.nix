{ writeShellScriptBin, callPackage }:
let
  get-aws-field = callPackage ./get-aws-field.nix { };
in
writeShellScriptBin "get-aws-access-key" ''
  export AWS_ACCESS_KEY_ID=$(${get-aws-field} "Access Key Id")
  export AWS_SECRET_ACCESS_KEY=$(${get-aws-field} "Secret Access Key")

  $@
''
