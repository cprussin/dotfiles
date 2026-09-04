{
  gzip,
  lib,
  python3,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation {
  pname = "read-dmarc";
  version = "1.0.0";

  src = ./read-dmarc.py;
  dontUnpack = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 $src $out/bin/read-dmarc
    substituteInPlace $out/bin/read-dmarc \
      --replace-fail '/usr/bin/env python3' '${python3}/bin/python3'
    runHook postInstall
  '';

  # Nothing here compiles, so `nix flake check` -- which only forces this to a
  # `drvPath` -- would never notice a broken install phase or a shebang that
  # points at nothing.  Running the tool over a report carrying one passing and
  # one failing record is what actually proves the thing works: it exercises
  # the archive handling, both verdict paths and the exit status at once.
  doInstallCheck = true;
  nativeInstallCheckInputs = [gzip];
  installCheckPhase = ''
    runHook preInstallCheck

    cat > report.xml <<'XML'
    <feedback>
      <report_metadata>
        <org_name>test</org_name>
        <date_range><begin>1756684800</begin><end>1756771199</end></date_range>
      </report_metadata>
      <policy_published><domain>example.com</domain><p>none</p></policy_published>
      <record>
        <row>
          <source_ip>192.0.2.1</source_ip>
          <count>2</count>
          <policy_evaluated>
            <disposition>none</disposition><dkim>pass</dkim><spf>pass</spf>
          </policy_evaluated>
        </row>
        <identifiers><header_from>example.com</header_from></identifiers>
        <auth_results>
          <dkim><domain>example.com</domain><result>pass</result></dkim>
        </auth_results>
      </record>
      <record>
        <row>
          <source_ip>192.0.2.2</source_ip>
          <count>3</count>
          <policy_evaluated>
            <disposition>none</disposition><dkim>fail</dkim><spf>fail</spf>
          </policy_evaluated>
        </row>
        <identifiers><header_from>bad.example</header_from></identifiers>
        <auth_results/>
      </record>
    </feedback>
    XML
    gzip -c report.xml > report.xml.gz

    $out/bin/read-dmarc --help > /dev/null

    status=0
    output=$($out/bin/read-dmarc --color never report.xml.gz) || status=$?
    echo "$output"

    # Failing mail present, so the exit status must be 1 and the failing
    # domain, not the passing one, must be what the summary leads with.
    [ "$status" -eq 1 ]
    echo "$output" | grep -q '3 of 5 messages failed'
    echo "$output" | grep -q 'bad.example'
    echo "$output" | grep -q 'message was not signed'
    echo "$output" | grep -q 'Passed all checks'

    runHook postInstallCheck
  '';

  meta = {
    description = "Summarize DMARC aggregate reports: what failed, and how";
    license = lib.licenses.mit;
    mainProgram = "read-dmarc";
    platforms = lib.platforms.all;
  };
}
