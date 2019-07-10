{ writeScript }:

writeScript "get-aws-access-key-nixops" ''
  getAwsField=@out@/bin/get-aws-field

  export EC2_ACCESS_KEY=$($getAwsField "Access Key Id")
  export EC2_SECRET_KEY=$($getAwsField "Secret Access Key")

  $@
''
