{ lib }: before: after:

lib.mkMerge [
  (lib.mkBefore before)
  (lib.mkAfter after)
]
