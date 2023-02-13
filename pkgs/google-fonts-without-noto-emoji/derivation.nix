{
  symlinkJoin,
  google-fonts,
}:
# google-fonts ships with broken NotoColorEmoji fonts, so let's remove them.
symlinkJoin {
  name = "google-fonts";
  paths = [google-fonts];
  postBuild = "rm $out/share/fonts/truetype/NotoColorEmoji{,CompatTest}-Regular.ttf";
}
