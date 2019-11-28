{ writeShellScript, config }: { folder, script }:

let
  exec = writeShellScript "email" ''
    cur=$(ls -1 $HOME/Mail/${folder}/Inbox/cur | wc -l)
    new=$(ls -1 $HOME/Mail/${folder}/Inbox/new | wc -l)
    total=$((cur + new))

    if [ $total -gt 0 ]
    then
      if [ $new -ne 0 ]
      then
        class="new"
      fi
      echo "{\"text\":\"${folder}  $total\",\"class\":\"$class\"}"
    fi
  '';
in

{
  name = "custom/email-${folder}";

  config = {
    inherit exec;
    on-click = script;
    interval = 1;
    return-type = "json";
  };

  style.".new".color = config.colorTheme.selection;
}
