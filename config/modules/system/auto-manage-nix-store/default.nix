{ ... }:

{
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
    };
    autoOptimiseStore = true;
  };
}
