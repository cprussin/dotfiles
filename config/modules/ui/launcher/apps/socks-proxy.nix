{
  writeShellScript,
  awscli,
  pass,
  gnugrep,
  gnused,
  openssh,
}: let
  aws = "${awscli}/bin/aws";
  pass_ = "${pass}/bin/pass";
  grep = "${gnugrep}/bin/grep";
  sed = "${gnused}/bin/sed";
  ssh = "${openssh}/bin/ssh";
  region = "eu-central-1";
  instance = "i-06977b5c76fe15596";
in
  writeShellScript "socks-proxy" ''
    readField() {
      ${grep} "^''${1}:" | ${sed} "s/^''${1}: //"
    }

    PASS=$(${pass_} show "Connor/Computer Services/AWS/jump instance runner")
    export AWS_ACCESS_KEY_ID=$(echo "$PASS" | readField "Access Key Id")
    export AWS_SECRET_ACCESS_KEY=$(echo "$PASS" | readField "Secret Access Key")

    echo "Starting jump host..."
    ${aws} ec2 start-instances --region ${region} --instance-ids ${instance}

    echo -e "\n\nWaiting for jump host to come online..."
    ${aws} ec2 wait instance-status-ok --region ${region} --instance-ids ${instance}

    echo -e "\n\nGetting jump host IP..."
    IP=$(${aws} ec2 describe-instances --region ${region} --instance-ids ${instance} --query 'Reservations[*].Instances[*].PublicIpAddress' --output text)

    echo -e "\n\nOpening SOCKS5 proxy..."
    ${ssh} -D 46756 ec2-user@$IP

    echo -e "\n\nStopping jump host..."
    ${aws} ec2 stop-instances --region ${region} --instance-ids ${instance}
  ''
