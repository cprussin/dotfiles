{
  config,
  pkgs,
  ...
}: let
  pkgs-unstable = import config.flake-inputs.nixpkgs-unstable {
    overlays = [];
    config = import ./nixpkgs-config.nix;
    inherit (pkgs.stdenv.hostPlatform) system;
  };
  unstable-pkgs-overlay = _: _: {
    inherit (pkgs-unstable) bitwig-studio zwave-js-server;

    # 26.05 is stuck on immich 2.7.5, which upstream stopped updating and
    # nixpkgs marks insecure (CVE-2026-59258, CVE-2026-82272), so crux no
    # longer evaluates at all with it.  immich 3 only reaches nixpkgs with
    # 26.11, and the honest fix is to run 3 rather than to write an exemption
    # for a server nobody is fixing anymore.
    #
    # Only the package moves, not the module.  26.05's `services.immich` and
    # unstable's differ by a `database.package` option nothing here sets, a
    # description, and one env var dropped from the machine-learning worker
    # -- nothing that tracks the major version -- so 26.05's module drives
    # 3.x as it stands.  The two channels also agree on vectorchord 1.1.1, so
    # the postgresql extensions that module installs are the ones this immich
    # asks for.
    #
    # The worker is named here as well as reached through the server's
    # `passthru.machine-learning`.  The unit uses the passthru, so it would
    # follow the server either way; taking the attribute too keeps a
    # `pkgs.immich-machine-learning` built from 26.05's recipe -- patches and
    # all -- from being handed 3.x's source, since it takes both its version
    # and its `src` from whatever `immich` is.
    #
    # Drop all of this when crux moves to 26.11 and stable carries immich 3.
    inherit (pkgs-unstable) immich immich-machine-learning;
  };
in {
  nix = {
    channel.enable = false;
    registry.nixpkgs.flake = config.flake-inputs.nixpkgs;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "-d";
    };
    settings = {
      trusted-substituters = [
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      experimental-features = "flakes nix-command";
      auto-optimise-store = true;
    };
  };

  nixpkgs = {
    config = import ./nixpkgs-config.nix;
    overlays = [
      unstable-pkgs-overlay
      (import ../../../../pkgs/dircolors-solarized/overlay.nix {
        src = config.flake-inputs.dircolors-solarized;
      })
      (import "${config.flake-inputs.fzf-pass}/overlay.nix")
      (import ../../../../pkgs/mako/overlay.nix)
      (import ../../../../pkgs/notify-send/overlay.nix {
        src = config.flake-inputs.notify-send;
      })
      (import ../../../../pkgs/pass-with-otp/overlay.nix)
      (import ../../../../pkgs/powerpanel/overlay.nix)
      (import ../../../../pkgs/read-dmarc/overlay.nix)
      (import ../../../../pkgs/sudo-with-insults/overlay.nix)
    ];
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
  primary-user.home-manager.xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
}
