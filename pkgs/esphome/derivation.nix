{ callPackage, fetchFromGitHub, nixpkgs, lib, python3, platformio, esptool, git, locale }:

let
  python = python3.override {
    packageOverrides = _: super: let
      mkOverride = { version, name, sha256 ? "", pname ? name, customOverrides ? _: {} }:
        super.${name}.overrideAttrs (
          oldAttrs: {
            inherit version;
            src = python3.pkgs.fetchPypi {
              inherit version pname sha256;
            };
          } // (customOverrides oldAttrs)
        );
    in
      {
        protobuf = super.protobuf.override {
          protobuf = callPackage "${nixpkgs}/pkgs/development/libraries/protobuf/generic-v3.nix" {
            version = "3.13.0";
            sha256 = "1nqsvi2yfr93kiwlinz8z7c68ilg1j75b2vcpzxzvripxx5h6xhd";
          };
        };

        pyyaml = mkOverride {
          name = "pyyaml";
          version = "5.3.1";
          pname = "PyYAML";
          sha256 = "0pb4zvkfxfijkpgd1b86xjsqql97ssf1knbd1v53wkg1qm9cgsmq";
        };

        pytz = mkOverride {
          name = "pytz";
          version = "2020.1";
          sha256 = "c35965d010ce31b23eeb663ed3cc8c906275d6be1a34393a1d73a41febf4a048";
        };

        ifaddr = mkOverride {
          name = "ifaddr";
          version = "0.1.7";
          sha256 = "1f9e8a6ca6f16db5a37d3356f07b6e52344f6f9f7e806d618537731669eb1a94";
        };

        click = mkOverride {
          name = "click";
          version = "7.1.2";
          sha256 = "d2b5255c7c6349bc1bd1e59e08cd12acbbd63ce649f2588755783aa94dfb6b1a";
          customOverrides = _: {
            postPatch = ''
              substituteInPlace src/click/_unicodefun.py \
                --replace "'locale'" "'${locale}/bin/locale'"
            '';
          };
        };

        colorlog = mkOverride {
          name = "colorlog";
          version = "4.4.0";
          sha256 = "1lg3gkwyyahsi72b0p9jr5iijrsvs5s5hlqmk4xydccs8qvwawh2";
        };

        tzlocal = mkOverride {
          name = "tzlocal";
          version = "2.1";
          sha256 = "643c97c5294aedc737780a49d9df30889321cbe1204eac2c2ec6134035a92e44";
        };

        tornado = mkOverride {
          name = "tornado";
          version = "6.0.4";
          sha256 = "0fe2d45ba43b00a41cd73f8be321a44936dc1aba233dee979f17a042b83eb6dc";
        };

        paho-mqtt = mkOverride {
          name = "paho-mqtt";
          version = "1.5.1";

          customOverrides = _: {
            src = fetchFromGitHub {
              owner = "eclipse";
              repo = "paho.mqtt.python";
              rev = "v1.5.1";
              sha256 = "1y537i6zxkjkmi80w5rvd18npz1jm5246i2x8p3q7ycx94i8ixs0";
            };
          };
        };
      };
  };

  pname = "esphome";
  #version = "1.15.3";
  version = "1.15.3-git";

in

python.pkgs.buildPythonApplication {
  inherit pname version;

  src = ../../../esphome;
  #python.pkgs.fetchPypi {
  #  inherit pname version;
  #  sha256 = "02xanwk03xgmd8pc1jxp2fy419rcc97yg3x1xjab7d5qdzkm6nx7";
  #};

  ESPHOME_USE_SUBPROCESS = "";

  propagatedBuildInputs = [
    python.pkgs.voluptuous
    python.pkgs.pyyaml
    python.pkgs.paho-mqtt
    python.pkgs.colorlog
    python.pkgs.tornado
    python.pkgs.protobuf
    python.pkgs.tzlocal
    python.pkgs.pyserial
    python.pkgs.ifaddr
    python.pkgs.click
  ];

  # remove all version pinning (E.g tornado==5.1.1 -> tornado)
  postPatch = "sed -i -e \"s/==[0-9.]*//\" setup.py";

  makeWrapperArgs = [
    # platformio is used in esphomeyaml/platformio_api.py
    # esptool is used in esphomeyaml/__main__.py
    # git is used in esphomeyaml/writer.py
    "--prefix PATH : ${lib.makeBinPath [ platformio esptool git ]}"
    "--set ESPHOME_USE_SUBPROCESS ''"
  ];

  # Platformio will try to access the network
  # Instead, run the executable
  checkPhase = "$out/bin/esphome --help > /dev/null";
}
