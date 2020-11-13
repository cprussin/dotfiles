{ fetchFromGitHub, lib, python3, platformio, esptool, git, protobuf3_13 }:
let
  python = python3.override {
    packageOverrides = _: super: {
      protobuf = super.protobuf.override {
        protobuf = protobuf3_13;
      };

      colorlog =
        let
          version = "4.6.2";
        in
        super.colorlog.overrideAttrs (_: {
          inherit version;

          src = python3.pkgs.fetchPypi {
            inherit version;
            pname = "colorlog";
            sha256 = "191f98indvnmax30y6i2hfykv6mi3lhc8c61hg1ay8lw859z3ral";
          };

          doInstallCheck = false;
        });

      paho-mqtt =
        let
          version = "1.5.1";
        in
        super.paho-mqtt.overrideAttrs (_: {
          inherit version;

          src = fetchFromGitHub {
            owner = "eclipse";
            repo = "paho.mqtt.python";
            rev = "v${version}";
            sha256 = "1y537i6zxkjkmi80w5rvd18npz1jm5246i2x8p3q7ycx94i8ixs0";
          };
        });

      pytz =
        let
          version = "2020.4";
        in
        super.pytz.overrideAttrs (_: {
          inherit version;

          src = python3.pkgs.fetchPypi {
            inherit version;
            pname = "pytz";
            sha256 = "0s72lz9q7rm2xgl2in0nvhn5cp0cyrxa257fpj2919g0s797ssry";
          };
        });

      voluptuous =
        let
          version = "0.12.0";
        in
        super.voluptuous.overrideAttrs (_: {
          inherit version;

          src = python3.pkgs.fetchPypi {
            inherit version;
            pname = "voluptuous";
            sha256 = "1p5j3fgbpqj31fajkaisdrz10ah9667sijz4kp3m0sbgw6ag4kis";
          };
        });
    };
  };

  pname = "esphome";
  version = "1.15.3-git";

in
python.pkgs.buildPythonApplication {
  inherit pname version;

  src = fetchFromGitHub {
    owner = "esphome";
    repo = "esphome";
    rev = "b493d5bba5d41d1c22b54efd4174bea6140fc177";
    sha256 = "0p9c9nn3afnnkxhywq6b43kjfl258smfsprdva4zkbpyqhqlla7c";
  };

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
