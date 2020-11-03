{ fetchFromGitHub, lib, python3, platformio, esptool, git, protobuf3_13 }:
let
  python = python3.override {
    packageOverrides = _: super: {
      protobuf = super.protobuf.override {
        protobuf = protobuf3_13;
      };

      colorlog =
        let
          version = "4.4.0";
        in
        super.colorlog.overrideAttrs (_: {
          inherit version;

          src = python3.pkgs.fetchPypi {
            inherit version;
            pname = "colorlog";
            sha256 = "1lg3gkwyyahsi72b0p9jr5iijrsvs5s5hlqmk4xydccs8qvwawh2";
          };
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
    };
  };

  pname = "esphome";
  version = "1.15.3-git";

in
python.pkgs.buildPythonApplication {
  inherit pname version;

  src = fetchFromGitHub {
    owner = "cprussin";
    repo = "esphome";
    rev = "set-tuya-mcu-minimum-brightness";
    sha256 = "0a6yy6w9gpw0hr89rjdn1bik10yf05s4lhqn9mbz3mjfh0x53by2";
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
