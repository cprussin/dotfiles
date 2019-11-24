{ writeShellScriptBin, callPackage, nixops }:

let
  get-aws-field = callPackage ./get-aws-field.nix {};
in

writeShellScriptBin "get-aws-access-key-nixops" ''
  export EC2_ACCESS_KEY=$(${get-aws-field} "Access Key Id")
  export EC2_SECRET_KEY=$(${get-aws-field} "Secret Access Key")

  ${nixops}/bin/nixops "$@"
''
