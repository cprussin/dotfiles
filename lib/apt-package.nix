# Picks the newest `package` out of an apt repository's `Packages` index and
# returns a `fetchurl` source for its .deb.  The index is a flake input, so
# `nix flake update` re-fetches it and the version and hash follow; nothing
# here hard-codes either.
#
# `indexes` maps a Nix system to that architecture's `Packages` file; `baseUrl`
# is the repository root, which the index's `Filename:` fields are relative to.
{
  lib,
  fetchurl,
  stdenv,
}: {
  package,
  baseUrl,
  indexes,
}: let
  index =
    indexes.${stdenv.hostPlatform.system}
    or (throw "${package} is not packaged for ${stdenv.hostPlatform.system}");

  # CRs stripped so a CRLF index can't silently merge every stanza into one.
  contents = lib.replaceStrings ["\r"] [""] (builtins.readFile "${index}");

  # Stanzas are blank-line separated `Key: value` lines.  Continuation lines
  # (leading whitespace, as in `Description:`) are dropped: no field read here
  # spans more than one line.
  parseStanza = stanza:
    lib.listToAttrs (
      lib.concatMap (
        line: let
          m = builtins.match "([^ \t:]+): *(.*)" line;
        in
          lib.optional (m != null) (lib.nameValuePair (lib.elemAt m 0) (lib.elemAt m 1))
      ) (lib.splitString "\n" stanza)
    );

  candidates =
    lib.filter (s: (s.Package or null) == package)
    (map parseStanza (lib.splitString "\n\n" contents));

  newest =
    if candidates == []
    then throw "no ${package} in the apt index ${index}"
    else
      lib.foldl' (
        a: b:
          if builtins.compareVersions b.Version a.Version > 0
          then b
          else a
      ) (lib.head candidates) (lib.tail candidates);
in {
  version = newest.Version;
  src = fetchurl {
    url = "${baseUrl}/${newest.Filename}";
    sha256 = newest.SHA256;
  };
}
