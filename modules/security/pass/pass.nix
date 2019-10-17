{ symlinkJoin, makeWrapper, pass, secure, stdenv, utillinux, sudo, gnugrep, coreutils }:

let
  pkg = passedExtensions: symlinkJoin {
    name = "pass";
    paths = [ (pass.withExtensions (exts: (passedExtensions exts) ++ [ exts.pass-otp ])) ];
    buildInputs = [ makeWrapper ];
    postBuild = ''
      mv $out/bin/pass $out/bin/.pass-original

      cat -- > $out/bin/pass <<-EOF
      #! ${stdenv.shell}

      mount="${utillinux}/bin/mount"
      umount="${utillinux}/bin/umount"
      sudo="/run/wrappers/bin/sudo"
      grep="${gnugrep}/bin/grep"
      echo="${coreutils}/bin/echo"

      export PASSWORD_STORE_DIR=\''${PASSWORD_STORE_DIR-${secure.passwords}}
      export GNUPGHOME=\''${GNUPGHOME-${secure.gnupg}}

      wasSecureMounted=\$(((\$mount | \$grep ${secure.path}) >/dev/null); \$echo \$?)

      if ! (exit \$wasSecureMounted)
      then
          \$sudo \$mount ${secure.path}
      fi

      out=\$($out/bin/.pass-original "\$@")

      if ! (exit \$wasSecureMounted)
      then
          \$sudo \$umount ${secure.path}
      fi

      \$echo "\$out"
      EOF

      chmod +x $out/bin/pass
    '';
  };
in

pkg (_: []) // {
  withExtensions = pkg;
}
