let
  region = "us-west-1";
  zone = "us-west-1a";
  publicAccess = { sourceIp = "0.0.0.0/0"; };
  tcpPort = fromPort:
    {
      inherit fromPort;
      protocol = "tcp";
      toPort = fromPort;
    };
in

{
  hydra = { resources, ... }: {
    deployment = {
      targetEnv = "ec2";
      ec2 = {
        inherit region zone;
        instanceType = "t3.micro";
        keyPair = "connor@prussin.net";
        elasticIPv4 = "52.9.139.186";
        ebsBoot = true;
        ebsInitialRootDiskSize = 10;
        securityGroups = [
          resources.ec2SecurityGroups.allow-public-ssh
        ];
        tags.Domain = "prussin.net";
      };
    };
  };

  resources = {
    ec2SecurityGroups = {
      allow-public-ssh = {
        inherit region;
        description = "Allow ssh.";
        rules = [ (publicAccess // tcpPort 22) ];
        tags.Domain = "prussin.net";
      };
    };
  };
}
