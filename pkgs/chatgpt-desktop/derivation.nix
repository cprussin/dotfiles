# OpenAI ships the Linux app as a .deb from their own apt repo and there's no
# nixpkgs package for it (`pkgs.chatgpt` is the macOS .dmg, darwin-only), so we
# unpack that .deb the same way `claude-desktop` does.  The URL layout and the
# per-arch hashes come from the AUR `chatgpt-desktop` PKGBUILD, which repackages
# the same binaries.
#
# `version` and `hash` move together; the pool this reads from is listed at
# https://persistent.oaistatic.com/codex-app-prod/linux/deb/dists/stable/main/binary-amd64/Packages
{
  lib,
  stdenv,
  fetchurl,
  alsa-lib,
  at-spi2-core,
  autoPatchelfHook,
  cairo,
  coreutils,
  cups,
  dbus,
  dpkg,
  expat,
  fontconfig,
  freetype,
  gdk-pixbuf,
  glib,
  graphite2,
  gtk3,
  libGL,
  libdrm,
  libgbm,
  libnotify,
  libpulseaudio,
  libsecret,
  libusb1,
  libuuid,
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
  pango,
  pipewire,
  python3,
  systemd,
  vulkan-loader,
  wayland,
  wrapGAppsHook3,
  xdg-utils,
}: let
  version = "26.820.71523";

  sources = {
    x86_64-linux = {
      debArch = "amd64";
      hash = "sha256-Ry0D6IophX8QFbK5F12AUjoTHPG8PpAX6xqP8jTeG9o=";
    };
    aarch64-linux = {
      debArch = "arm64";
      hash = "sha256-4OyqrqaqROfkWMky9c12EueGwK6wi+2XHLlq48QmYBM=";
    };
  };

  source =
    sources.${stdenv.hostPlatform.system}
    or (throw "chatgpt-desktop is not packaged for ${stdenv.hostPlatform.system}");

  # coreutils is load-bearing: the asar patch below makes the app copy its
  # bundled plugins with `cp` and `chmod` rather than node's fs.cp.
  runtimeBins = [
    coreutils
    xdg-utils
  ];

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
    graphite2
    gtk3
    libGL
    libdrm
    libgbm
    libnotify
    libpulseaudio
    libsecret
    libusb1
    libuuid
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
    stdenv.cc.cc.lib
    systemd
    vulkan-loader
    wayland
  ];
in
  stdenv.mkDerivation {
    pname = "chatgpt-desktop";
    inherit version;

    src = fetchurl {
      url = "https://persistent.oaistatic.com/codex-app-prod/linux/deb/pool/main/c/chatgpt/chatgpt_${version}_${source.debArch}.deb";
      inherit (source) hash;
    };

    nativeBuildInputs = [
      autoPatchelfHook
      dpkg
      makeWrapper
      python3
      wrapGAppsHook3
    ];

    buildInputs = runtimeLibs;

    # Electron dlopens these rather than linking them, so autoPatchelf has to
    # be told: they go on each executable's RPATH.  What they must not do is
    # arrive as an LD_LIBRARY_PATH on the app's environment -- see the wrapper
    # below.
    runtimeDependencies = [
      libGL
      libgbm
      libnotify
      libpulseaudio
      libsecret
      pipewire
      vulkan-loader
      wayland
      # systemd has no `lib` output at this nixpkgs rev, so this resolves to
      # the same path as bare `systemd` and is only here to stay correct if one
      # is ever added.  libudev.so.1 does live in its $out/lib, which is where
      # the hook looks: it appends /lib to whatever path it is given.
      (lib.getLib systemd)
    ];

    # Libraries the .deb ships that nothing here will load: the Qt shims
    # Electron picks at runtime to match a Qt desktop's theme (this one is
    # GTK), and the musl and Android builds of a few node addons, which sit
    # beside the glibc ones that actually get loaded.  Listed out rather than
    # blanket-ignored, so a library that does matter still fails the build.
    autoPatchelfIgnoreMissingDeps = [
      "libQt5Core.so.5"
      "libQt5Gui.so.5"
      "libQt5Widgets.so.5"
      "libQt6Core.so.6"
      "libQt6Gui.so.6"
      "libQt6Widgets.so.6"
      "libc++_shared.so"
      "libc.musl-aarch64.so.1"
      "libc.musl-x86_64.so.1"
      "liblog.so"
    ];

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

      mkdir -p "$out/bin" "$out/lib" "$out/share"
      cp -a usr/lib/chatgpt "$out/lib/"
      cp -a usr/share/applications usr/share/doc "$out/share/"

      # This .deb ships no icon theme dir today, and where it would put one if
      # that changes isn't worth guessing at.
      for dir in icons pixmaps
      do
        if test -d "usr/share/$dir"
        then
          cp -a "usr/share/$dir" "$out/share/"
        fi
      done

      for desktop in "$out"/share/applications/*.desktop
      do
        substituteInPlace "$desktop" \
          --replace-fail "Exec=chatgpt" "Exec=$out/bin/chatgpt"
      done

      # Two things the app does that only break on NixOS: it materializes its
      # bundled plugins into ~/.codex with node's fs.cp, which preserves the
      # store's read-only modes and then can't write the manifests it rewrites
      # (EACCES on every launch), and @parcel/watcher probes `process.report`,
      # which trips a CFI guard in the bundled Electron runtime.  Both patches
      # fail the build if the code they match has moved.
      python3 ${./patch-asar.py} "$out/lib/chatgpt/resources/app.asar"

      # `codex-launcher` is upstream's entry point, not the ChatGPT binary
      # beside it: a two-line script that resolves its sibling from its own
      # path and execs it.  The wrapper goes on that sibling because ChatGPT is
      # the process that needs the flags and the environment, and wrapping it
      # also covers anything that runs it directly rather than via the
      # launcher.
      test -e "$out/lib/chatgpt/codex-launcher" \
        || (echo "no codex-launcher in this .deb -- wrap ChatGPT directly" >&2; exit 1)
      ln -s ../lib/chatgpt/codex-launcher "$out/bin/chatgpt"

      runHook postInstall
    '';

    # The ozone flag rather than ELECTRON_OZONE_PLATFORM_HINT, which this
    # Electron fork ignores; guarded on WAYLAND_DISPLAY so an X11 session is
    # unaffected.  Being flagsBefore, a `--ozone-platform=` passed on the
    # command line still wins, which is the way back to XWayland.
    preFixup = ''
      gappsWrapperArgs+=(
        # --suffix, not --prefix (which is what claude-desktop uses): every
        # child inherits this, including the codex agent shelling out in a
        # project of mine, and it should get my coreutils rather than the
        # app's.  The store copy is only the fallback that makes the asar
        # patch's `cp`/`chmod` work when PATH has neither.
        --suffix PATH : ${lib.makeBinPath runtimeBins}
        --add-flags "\''${WAYLAND_DISPLAY:+--ozone-platform=wayland}"
      )
    '';

    # `wrapProgramShell`, not `wrapProgram`: wrapGAppsHook3 propagates
    # makeBinaryWrapper, which redefines `wrapProgram` to emit a C wrapper that
    # passes --add-flags through verbatim -- the `${WAYLAND_DISPLAY:+...}` above
    # would reach Electron as a literal argument instead of expanding.  The
    # shell flavour stays available because makeWrapper is in nativeBuildInputs.
    postFixup = ''
      wrapProgramShell "$out/lib/chatgpt/ChatGPT" "''${gappsWrapperArgs[@]}"
    '';

    meta = {
      description = "Official ChatGPT desktop app for Linux";
      homepage = "https://chatgpt.com/download";
      license = lib.licenses.unfree;
      mainProgram = "chatgpt";
      platforms = builtins.attrNames sources;
      sourceProvenance = [lib.sourceTypes.binaryNativeCode];
    };
  }
