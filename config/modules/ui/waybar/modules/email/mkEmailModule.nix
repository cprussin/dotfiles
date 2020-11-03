{ writeShellScript, mu, colors }: { folder, script }:
let
  exec = writeShellScript "email" ''
    mu=${mu}/bin/mu

    new=$($mu find maildir:"/${folder}/Inbox" flag:new 2>/dev/null | wc -l)
    total=$($mu find maildir:"/${folder}/Inbox" 2>/dev/null | wc -l)

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

  style.".new".color = colors.selection;
}
