# Anthropic ships the Linux app as a .deb from their own apt repo and there's
# no nixpkgs package for it, so we unpack that .deb ourselves.  The recipe --
# in particular the two paths patched inside app.asar, without which Cowork
# can't find QEMU's firmware -- is adapted from
# https://github.com/poeck/claude-desktop-nix-flake (MIT).
#
# Version and hash come from the apt index, a flake input: `nix flake update`
# picks up new releases.  See lib/apt-package.nix.
{
  lib,
  stdenv,
  callPackage,
  aptIndexes,
  alsa-lib,
  asar,
  at-spi2-core,
  autoPatchelfHook,
  cairo,
  cups,
  dbus,
  dpkg,
  expat,
  fontconfig,
  freetype,
  gdk-pixbuf,
  glib,
  gtk3,
  libGL,
  libayatana-appindicator,
  libcap_ng,
  libdrm,
  libgbm,
  libnotify,
  libpulseaudio,
  libseccomp,
  libsecret,
  libuuid,
  libva,
  libx11,
  libxcb,
  libxcomposite,
  libxcursor,
  libxdamage,
  libxext,
  libxfixes,
  libxi,
  libxkbcommon,
  libxrandr,
  libxrender,
  libxscrnsaver,
  libxtst,
  makeWrapper,
  mesa,
  nspr,
  nss,
  OVMF,
  pango,
  perl,
  pipewire,
  qemu,
  systemd,
  trash-cli,
  vulkan-loader,
  wayland,
  wrapGAppsHook3,
  xdg-utils,
}: let
  inherit
    (callPackage ../../lib/apt-package.nix {} {
      package = "claude-desktop";
      baseUrl = "https://downloads.claude.ai/claude-desktop/apt/stable";
      indexes = aptIndexes;
    })
    version
    src
    ;

  firmwareCodePath =
    if stdenv.hostPlatform.isAarch64
    then "${qemu}/share/qemu/edk2-aarch64-code.fd"
    else "${OVMF.fd}/FV/OVMF_CODE.fd";

  runtimeLibs = [
    alsa-lib
    at-spi2-core
    cairo
    cups
    dbus
    expat
    fontconfig
    freetype
    gdk-pixbuf
    glib
    gtk3
    libGL
    libayatana-appindicator
    libcap_ng
    libdrm
    libgbm
    libnotify
    libpulseaudio
    libseccomp
    libsecret
    libuuid
    libva
    libx11
    libxcb
    libxcomposite
    libxcursor
    libxdamage
    libxext
    libxfixes
    libxi
    libxkbcommon
    libxrandr
    libxrender
    libxscrnsaver
    libxtst
    mesa
    nspr
    nss
    pango
    # @ant/claude-native links it.
    pipewire
    stdenv.cc.cc.lib
    systemd
    vulkan-loader
    wayland
  ];

  runtimeBins = [
    glib
    qemu
    trash-cli
    xdg-utils
  ];
in
  stdenv.mkDerivation {
    pname = "claude-desktop";
    inherit version;

    inherit src;

    nativeBuildInputs = [
      asar
      autoPatchelfHook
      dpkg
      makeWrapper
      perl
      wrapGAppsHook3
    ];

    buildInputs = runtimeLibs;

    dontConfigure = true;
    dontBuild = true;
    dontStrip = true;
    dontWrapGApps = true;

    unpackPhase = ''
      runHook preUnpack
      dpkg-deb --fsys-tarfile "$src" | tar --extract --file - --no-same-permissions
      runHook postUnpack
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p "$out/lib" "$out/share"
      cp -a usr/lib/claude-desktop "$out/lib/"
      cp -a usr/share/applications usr/share/icons usr/share/doc "$out/share/"

      for desktop in "$out"/share/applications/*.desktop
      do
        substituteInPlace "$desktop" \
          --replace-fail "Exec=claude-desktop" "Exec=$out/bin/claude-desktop"
      done

      # The .deb bundles virtiofsd for distros that don't package it; the patch
      # below points Cowork at that copy, and unlike the three rewrites it has
      # nothing to fail against if a release drops it.  If this ever fires,
      # nixpkgs has a `virtiofsd` to point at instead.
      test -e "$out/lib/claude-desktop/resources/virtiofsd" \
        || (echo "no bundled virtiofsd in this .deb -- use pkgs.virtiofsd" >&2; exit 1)

      asarRoot="$(mktemp -d)"
      asar extract "$out/lib/claude-desktop/resources/app.asar" "$asarRoot"

      # The VM code lives in a hashed chunk under .vite/build as of 2.x, not
      # index.js, so find it by the path it's about to have patched out.
      vmChunk="$(grep -l -F '/usr/share/OVMF/OVMF_CODE_4M.fd' "$asarRoot"/.vite/build/*.js || true)"
      test -f "$vmChunk" \
        || (echo "could not identify the Claude Desktop VM code chunk" >&2; exit 1)

      FIRMWARE_CODE_PATH="${firmwareCodePath}" \
      VIRTIOFSD_PATH="$out/lib/claude-desktop/resources/virtiofsd" \
      perl -0pi -e '
        s{([A-Za-z0-9_\$]+)=process\.arch==="arm64"\?\["/usr/share/AAVMF/AAVMF_CODE\.fd"\]:\["/usr/share/OVMF/OVMF_CODE_4M\.fd","/usr/share/OVMF/OVMF_CODE\.fd"\]}{$1=["$ENV{FIRMWARE_CODE_PATH}"]} or die "failed to patch firmware path\n";
        s{([A-Za-z0-9_\$]+)=\["/usr/libexec/virtiofsd","/usr/bin/virtiofsd"\]}{$1=["$ENV{VIRTIOFSD_PATH}"]} or die "failed to patch virtiofsd path\n";
        s{return ([A-Za-z0-9_\$]+)\.replace\("OVMF_CODE","OVMF_VARS"\)\.replace\("AAVMF_CODE","AAVMF_VARS"\)}{return $1.replace("OVMF_CODE","OVMF_VARS").replace("AAVMF_CODE","AAVMF_VARS").replace("edk2-aarch64-code.fd","edk2-arm-vars.fd")} or die "failed to patch firmware vars path\n";
      ' "$vmChunk"

      rm "$out/lib/claude-desktop/resources/app.asar"
      asar pack --unpack "*.node" "$asarRoot" "$out/lib/claude-desktop/resources/app.asar"

      runHook postInstall
    '';

    preFixup = ''
      gappsWrapperArgs+=(
        --prefix PATH : ${lib.makeBinPath runtimeBins}
        --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath runtimeLibs}
        --set-default ELECTRON_OZONE_PLATFORM_HINT auto
      )
    '';

    postFixup = ''
      makeWrapper "$out/lib/claude-desktop/claude-desktop" "$out/bin/claude-desktop" \
        "''${gappsWrapperArgs[@]}"
    '';

    meta = {
      description = "Official Claude Desktop Linux beta";
      homepage = "https://claude.ai";
      changelog = "https://code.claude.com/docs/en/desktop-linux";
      license = lib.licenses.unfree;
      mainProgram = "claude-desktop";
      platforms = builtins.attrNames aptIndexes;
      sourceProvenance = [lib.sourceTypes.binaryNativeCode];
    };
  }
