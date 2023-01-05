{
  trivialBuild,
  callPackage,
  epkgs,
}:
(trivialBuild {
  pname = "emacs-rc";
  version = "0.0.1";
  src = ./.;
  packageRequires = callPackage ./dependencies.nix {inherit epkgs;};

  # TODO nixpkgs doesn't need to be byte-compiling if we're going to native
  # compile anyways, I should put in a patch to disable this if native
  # compilation is enabled:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/emacs/trivial.nix#L10
  buildPhase = null;
})
#
# HACK HACK HACK
#
# There is a bug in emacs where `batch-native-compile` doesn't properly remove
# filenames from the argument list, so after native compiling,
# `command-line-1` tries to load the arguments.  But we don't want to be
# loading any buffers while compiling, both because it's a waste of time, and
# also because that will cause hooks to run that we don't necessarily have the
# right environment set up for (i.e. trying to start flyspell when there's no
# ispell available).
#
# This hack just inserts a `(lambda () t)` to `command-line-functions` in the
# command that calls `batch-native-compile`, so that emacs considers all
# command line arguments to be handled and does not try to load any buffers.
#
# TODO I really should put in a bug report / patch to emacs, but the emacs
# contribution process assumes emacs is set up to send mail, which will take
# some work that I don't want to deal with right now.
#
.overrideAttrs (attrs: {
  postInstall =
    builtins.replaceStrings
    ["--eval '(setq large-file-warning-threshold nil)'"]
    ["--eval '(setq large-file-warning-threshold nil command-line-functions (list (lambda () t)))'"]
    attrs.postInstall or "";
})
