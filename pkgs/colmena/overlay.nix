# Colmena spawns the key uploader before running `keyCommand`.  When the
# command fails (e.g. a gpg prompt left too long), the uploader sees EOF and
# replaces the key with an empty file.  Read the key first.
_: prev: {
  colmena = prev.colmena.overrideAttrs (old: {
    patches = (old.patches or []) ++ [./read-key-before-spawning-uploader.patch];
  });
}
