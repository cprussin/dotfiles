{lib}:
lib.types.submodule {
  options = {
    layout = lib.mkOption {
      description = "Keymap";
      type = lib.types.str;
    };

    variant = lib.mkOption {
      description = "Keymap variant, e.g. \"dvp\".  Empty is the layout's own.";
      type = lib.types.str;
      default = "";
    };

    options = lib.mkOption {
      description = "Options to apply to the keymap, e.g. [\"caps:escape\"]";
      # A LIST BECAUSE THE XKB FORMAT IS ONE.  sway and ckbcomp both want the
      # comma-separated line, so they join it where they are written -- which
      # is one join each at a boundary that already exists, rather than every
      # other reader splitting a string back into the list it started as.
      # domicile's config takes the list outright.
      type = lib.types.listOf lib.types.str;
      default = [];
    };
  };
}
