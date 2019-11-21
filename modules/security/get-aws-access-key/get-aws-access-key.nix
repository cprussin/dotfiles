{ writeShellScript }:

writeShellScript "get-aws-access-key" ''
  getAwsField=@out@/bin/get-aws-field

  export AWS_ACCESS_KEY_ID=$($getAwsField "Access Key Id")
  export AWS_SECRET_ACCESS_KEY=$($getAwsField "Secret Access Key")

  $@
''
