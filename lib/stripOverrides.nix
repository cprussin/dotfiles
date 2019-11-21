{ lib }:

lib.filterAttrs (name: _: name != "override" && name != "overrideDerivation")
