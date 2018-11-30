{ callPackage, python, fetchurl }:

let
  requirements = callPackage ./requirements.nix { };
in

python.pkgs.buildPythonPackage rec {
  name = "metatron-python-${version}";
  version = "0.19.30";
  doCheck = false;

  propagatedBuildInputs = builtins.attrValues requirements.packages;

  src = fetchurl {
    url = "https://artifacts.netflix.com/pypi-netflix/metatron/metatron-0.19.30.tar.gz";
    sha256 = "06q0zmjh4248k0xwvcq53bydf5gfqj5g375c1az6vdiyhxwczx9q";
  };
}
