{}:

let
  pass-data = builtins.fromJSON (builtins.getEnv "PASS_DATA");
in

rec {
  get-password-field = pass: field: pass-data."${pass}"."${field}";
  get-password = pass: get-password-field pass "Password";
  public-ssh-key = pass-data.public-ssh-key;
}
