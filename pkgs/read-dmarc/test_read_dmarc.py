"""Tests for read-dmarc's source classification.

Run as `python3 test_read_dmarc.py`.  `READ_DMARC` names the script under test,
so the build can point it at the installed copy; it defaults to the one beside
this file.
"""

import importlib.machinery
import importlib.util
import os
import socket
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

SCRIPT = os.environ.get("READ_DMARC") or str(Path(__file__).with_name("read-dmarc.py"))
loader = importlib.machinery.SourceFileLoader("read_dmarc", SCRIPT)
spec = importlib.util.spec_from_loader("read_dmarc", loader)
rd = importlib.util.module_from_spec(spec)
# Registered first, as `import` would: dataclasses look their module up here.
sys.modules["read_dmarc"] = rd
loader.exec_module(rd)

AVANAN_IP = "35.174.145.124"
AVANAN_HOST = "us.cloud-sec-av.com"


def report(*records: str) -> bytes:
    return f"""
    <feedback>
      <report_metadata>
        <org_name>test</org_name>
        <date_range><begin>1756684800</begin><end>1756771199</end></date_range>
      </report_metadata>
      <policy_published><domain>example.com</domain><p>reject</p></policy_published>
      {''.join(records)}
    </feedback>
    """.encode()


def record(
    ip: str,
    spf: str = "fail",
    dkim: str = "fail",
    header_from: str = "example.com",
    auth: str = "",
    reason: str = "",
    count: int = 1,
) -> str:
    return f"""
    <record>
      <row>
        <source_ip>{ip}</source_ip>
        <count>{count}</count>
        <policy_evaluated>
          <disposition>none</disposition><dkim>{dkim}</dkim><spf>{spf}</spf>
          {reason}
        </policy_evaluated>
      </row>
      <identifiers><header_from>{header_from}</header_from></identifiers>
      <auth_results>{auth}</auth_results>
    </record>
    """


def dkim_auth(domain: str, result: str) -> str:
    return f"<dkim><domain>{domain}</domain><selector>s1</selector><result>{result}</result></dkim>"


def spf_auth(domain: str, result: str) -> str:
    return f"<spf><domain>{domain}</domain><result>{result}</result></spf>"


def parse_one(xml: str):
    _, records, _ = rd.parse(report(xml))
    return records[0]


def fake_dns(ptr: dict, forward: dict):
    """Patch the resolver: `ptr` maps IP to names, `forward` name to IPs."""

    def gethostbyaddr(ip):
        if ip not in ptr:
            raise socket.herror(1, "Unknown host")
        name, *aliases = ptr[ip]
        return name, aliases, [ip]

    def getaddrinfo(host, port):
        host = host.rstrip(".")
        if host not in forward:
            raise socket.gaierror(socket.EAI_NONAME, "Name or service not known")
        return [(socket.AF_INET, socket.SOCK_STREAM, 6, "", (ip, 0)) for ip in forward[host]]

    return mock.patch.multiple(
        rd.socket, gethostbyaddr=gethostbyaddr, getaddrinfo=getaddrinfo
    )


def classify_with_dns(rec, ptr, forward):
    with fake_dns(ptr, forward):
        names = rd.resolve([rec.ip], rd.DnsCache(None))
    rd.classify(rec, names[rec.ip])
    return rec


FORWARDED = record(
    AVANAN_IP,
    auth=spf_auth("example.com", "fail") + dkim_auth("example.com", "fail"),
)


class Classify(unittest.TestCase):
    def test_confirmed_scanner_is_known_forwarder(self):
        rec = classify_with_dns(
            parse_one(FORWARDED),
            {AVANAN_IP: ["ip-10-0-0-1.us.cloud-sec-av.com."]},
            {"ip-10-0-0-1.us.cloud-sec-av.com": [AVANAN_IP]},
        )
        self.assertEqual(rec.category, rd.KNOWN_FORWARDER)
        self.assertIn("Avanan", rec.vendor)
        self.assertFalse(rec.aligned_dkim_pass)

    def test_unconfirmed_ptr_is_not_known_forwarder(self):
        # Whoever owns the IP writes its PTR; only the forward zone vouches.
        rec = classify_with_dns(
            parse_one(FORWARDED),
            {AVANAN_IP: [AVANAN_HOST]},
            {AVANAN_HOST: ["192.0.2.99"]},
        )
        self.assertNotEqual(rec.category, rd.KNOWN_FORWARDER)
        self.assertEqual(rec.category, rd.UNKNOWN)
        self.assertEqual(rec.hostnames, ())

    def test_failed_lookup_is_not_known_forwarder(self):
        rec = parse_one(FORWARDED)

        def broken(ip):
            raise socket.herror(2, "Host name lookup failure")

        names = rd.resolve([rec.ip], rd.DnsCache(None), lookup=broken)
        self.assertIsNone(names[rec.ip])
        rd.classify(rec, names[rec.ip])
        self.assertEqual(rec.category, rd.UNKNOWN)

    def test_suffix_matches_on_label_boundary(self):
        rec = classify_with_dns(
            parse_one(FORWARDED),
            {AVANAN_IP: ["evilcloud-sec-av.com"]},
            {"evilcloud-sec-av.com": [AVANAN_IP]},
        )
        self.assertEqual(rec.category, rd.UNKNOWN)

    def test_scanner_that_kept_dkim_intact_is_noted(self):
        # Reporter says DKIM failed alignment, yet our signature verified.
        rec = classify_with_dns(
            parse_one(record(AVANAN_IP, auth=dkim_auth("example.com", "pass"))),
            {AVANAN_IP: [AVANAN_HOST]},
            {AVANAN_HOST: [AVANAN_IP]},
        )
        self.assertEqual(rec.category, rd.KNOWN_FORWARDER)
        self.assertTrue(rec.aligned_dkim_pass)

    def test_transient_forward_failure_is_not_an_answer(self):
        def getaddrinfo(host, port):
            raise socket.gaierror(socket.EAI_AGAIN, "Temporary failure")

        with mock.patch.multiple(
            rd.socket,
            gethostbyaddr=lambda ip: (AVANAN_HOST, [], [ip]),
            getaddrinfo=getaddrinfo,
        ):
            cache = rd.DnsCache(None)
            self.assertEqual(rd.resolve([AVANAN_IP], cache), {AVANAN_IP: None})
        self.assertIsNone(cache.get(AVANAN_IP))

    def test_aligned_dkim_from_unlisted_host_is_likely_forwarded(self):
        rec = classify_with_dns(
            parse_one(
                record(
                    "198.51.100.7",
                    header_from="news.example.com",
                    auth=spf_auth("relay.example.net", "fail")
                    + dkim_auth("example.com", "pass"),
                    reason="<reason><type>local_policy</type>"
                    "<comment>arc=pass as.1.relay.example.net=pass</comment></reason>",
                )
            ),
            {"198.51.100.7": ["mx.relay.example.net"]},
            {"mx.relay.example.net": ["198.51.100.7"]},
        )
        self.assertEqual(rec.category, rd.LIKELY_FORWARDED)
        self.assertEqual(rec.arc, ("pass",))

    def test_unaligned_dkim_is_unknown(self):
        rec = parse_one(record("203.0.113.5", auth=dkim_auth("attacker.example", "pass")))
        rd.classify(rec, [])
        self.assertEqual(rec.category, rd.UNKNOWN)

    def test_aligned_pass_is_pass(self):
        rec = parse_one(
            record(
                "192.0.2.1",
                spf="pass",
                dkim="pass",
                auth=spf_auth("example.com", "pass") + dkim_auth("example.com", "pass"),
            )
        )
        rd.classify(rec, None)
        self.assertEqual(rec.category, rd.PASS)

    def test_random_failure_is_unknown(self):
        rec = classify_with_dns(
            parse_one(record("203.0.113.9", auth=spf_auth("example.com", "fail"))),
            {"203.0.113.9": ["host.example.org"]},
            {"host.example.org": ["203.0.113.9"]},
        )
        self.assertEqual(rec.category, rd.UNKNOWN)
        self.assertEqual(rec.hostnames, ("host.example.org",))


class Alignment(unittest.TestCase):
    def test_relaxed(self):
        self.assertTrue(rd.aligned("example.com", "example.com", "example.com"))
        self.assertTrue(rd.aligned("mail.example.com", "example.com", ""))
        self.assertTrue(rd.aligned("mail.example.com", "news.example.com", "example.com"))
        self.assertFalse(rd.aligned("mail.example.com", "news.example.com", ""))
        self.assertFalse(rd.aligned("example.net", "example.com", "example.com"))
        self.assertFalse(rd.aligned("com", "example.com", ""))
        # A policy published at a subdomain still aligns its parent.
        self.assertTrue(rd.aligned("example.com", "news.example.com", "news.example.com"))


class Cache(unittest.TestCase):
    def test_ttl_and_round_trip(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "sub" / "rdns.json"
            clock = [1000.0]
            cache = rd.DnsCache(path, now=lambda: clock[0])
            cache.put("192.0.2.1", ["host.example.com"])
            cache.put("192.0.2.2", [])
            cache.save()

            reloaded = rd.DnsCache(path, now=lambda: clock[0])
            self.assertEqual(reloaded.get("192.0.2.1"), ["host.example.com"])
            self.assertEqual(reloaded.get("192.0.2.2"), [])
            clock[0] += rd.DNS_NEGATIVE_TTL
            self.assertEqual(reloaded.get("192.0.2.1"), ["host.example.com"])
            self.assertIsNone(reloaded.get("192.0.2.2"))
            clock[0] += rd.DNS_TTL
            self.assertIsNone(reloaded.get("192.0.2.1"))

    def test_cached_answer_skips_lookup(self):
        cache = rd.DnsCache(None)
        cache.put(AVANAN_IP, [AVANAN_HOST])

        def unexpected(ip):
            raise AssertionError("looked up a cached IP")

        self.assertEqual(rd.resolve([AVANAN_IP], cache, lookup=unexpected), {AVANAN_IP: [AVANAN_HOST]})

    def test_failures_are_not_cached(self):
        cache = rd.DnsCache(None)

        def broken(ip):
            raise OSError("timed out")

        rd.resolve(["192.0.2.1"], cache, lookup=broken)
        self.assertIsNone(cache.get("192.0.2.1"))

    def test_garbage_file_is_ignored(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "rdns.json"
            path.write_text('{"192.0.2.1": {"hosts": ["bad\\u001bname"], "at": 1}}')
            cache = rd.DnsCache(path, now=lambda: 2)
            self.assertEqual(cache.get("192.0.2.1"), [])
            path.write_text("not json")
            self.assertEqual(rd.DnsCache(path).entries, {})


class Render(unittest.TestCase):
    def summarize(self, xml: bytes, show_all: bool = False):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "report.xml"
            path.write_bytes(xml)
            domains, reports, problems = rd.collect([path])
        with fake_dns({AVANAN_IP: [AVANAN_HOST]}, {AVANAN_HOST: [AVANAN_IP]}), mock.patch.object(
            rd, "default_cache_path", return_value=None
        ):
            rd.classify_all(domains, True)
        return rd.render(domains, reports, problems, rd.Style(False), show_all)

    def test_forwarders_collapse_and_do_not_fail_the_run(self):
        xml = report(
            record("192.0.2.1", spf="pass", dkim="pass", count=10),
            record(AVANAN_IP, count=3),
            record(AVANAN_IP, auth=dkim_auth("example.com", "pass"), count=2),
        )
        text, code = self.summarize(xml)
        self.assertEqual(code, 0)
        self.assertIn("the other 5 were re-sent by known forwarders", text)
        self.assertIn("5 messages, DKIM 2 pass / 3 fail, 2025-09-01, for example.com", text)
        self.assertNotIn(AVANAN_IP, text)

        text, _ = self.summarize(xml, show_all=True)
        self.assertIn(AVANAN_IP, text)
        self.assertIn(AVANAN_HOST, text)

    def test_unknown_comes_first(self):
        xml = report(
            record(AVANAN_IP, count=3),
            record("198.51.100.7", auth=dkim_auth("example.com", "pass")),
            record("203.0.113.9", header_from="example.com"),
        )
        text, code = self.summarize(xml)
        self.assertEqual(code, 1)
        self.assertIn("5 of 5 messages failed DMARC (100.0%), 3 of them re-sent", text)
        unknown = text.index("Unknown sources")
        self.assertLess(unknown, text.index("Likely forwarded"))
        self.assertLess(text.index("Likely forwarded"), text.index("Known forwarders"))
        self.assertLess(unknown, text.index("203.0.113.9"))

    def test_all_lists_passes_once(self):
        xml = report(
            record("192.0.2.1", spf="pass", dkim="pass"),
            record("198.51.100.7", auth=dkim_auth("example.com", "pass")),
            record("203.0.113.9"),
        )
        text, _ = self.summarize(xml, show_all=True)
        self.assertEqual(text.count("192.0.2.1"), 1)


if __name__ == "__main__":
    unittest.main()
