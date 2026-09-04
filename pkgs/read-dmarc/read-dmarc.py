#!/usr/bin/env python3
"""Summarize DMARC aggregate reports.

Reads DMARC aggregate (RUA) reports -- raw XML, or the zip/gzip/tar.gz
envelopes reporters actually mail them in -- and answers the question that
matters first: did everything pass, and if not, which domains failed and how.
Domains that passed every check are summarized at the end, out of the way.
"""

from __future__ import annotations

import argparse
import gzip
import io
import os
import stat
import sys
import tarfile
import zipfile
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from xml.etree import ElementTree

# The offset and magic that POSIX tar writes into every header block.  Checking
# it lets us tell a bare `.xml.gz` from a `.tar.gz` by content rather than by
# trusting the file name, which reporters are not consistent about.
TAR_MAGIC_OFFSET = 257
TAR_MAGIC = b"ustar"

# These files arrive as mail attachments from arbitrary third parties, so the
# decompressed size is not ours to trust: cap it rather than let a few KB of
# crafted gzip expand until the machine runs out of memory.  Real aggregate
# reports are a few MB at the very top end.
MAX_DECOMPRESSED = 256 * 1024 * 1024
LIMIT_DESCRIPTION = "256 MiB"

# Reporters nest one archive at most (a .xml.gz inside a .zip); this stops a
# few levels past that, where it is either a mistake or someone being clever
# at our expense.  Counted in containers opened, so 4 admits four of them.
MAX_NESTING = 4

# Timestamps outside this range are the reporter's bug, not a date: the usual
# culprit is milliseconds where the schema says seconds, which overflows
# `datetime` outright.
MIN_TIMESTAMP = 0
MAX_TIMESTAMP = 32503680000  # 3000-01-01

# A million-member archive should not cost a million strings just to say that
# each member was empty, and nobody reads past the first few anyway.
MAX_PROBLEMS_LISTED = 20

# Longest run of stranger-written text we will echo.  Comfortably past the 253
# characters a domain name can hold, and short of what a `<org_name>` of 200 MB
# would otherwise put on the terminal.
MAX_TEXT = 500


class Style:
    """ANSI escapes, or empty strings when color is off."""

    NAMES = {
        "bold": "1",
        "dim": "2",
        "red": "31",
        "green": "32",
        "yellow": "33",
    }

    def __init__(self, enabled: bool):
        self.enabled = enabled

    def __call__(self, text: str, *names: str) -> str:
        if not self.enabled or not names:
            return text
        codes = ";".join(self.NAMES[name] for name in names)
        return f"\033[{codes}m{text}\033[0m"


@dataclass(frozen=True)
class Report:
    """One `<feedback>` document: who sent it and what it covers."""

    org: str
    domain: str
    policy: str
    subdomain_policy: str
    begin: int | None
    end: int | None


@dataclass
class Record:
    """One `<record>`: a batch of messages from a single source IP."""

    domain: str
    policy: str
    ip: str
    count: int
    disposition: str
    dkim: str
    spf: str
    dkim_auth: list[tuple[str, str, str]]
    spf_auth: list[tuple[str, str]]
    overrides: list[tuple[str, str]]

    @property
    def passed(self) -> bool:
        # DMARC passes when *either* identifier aligns and authenticates, so a
        # record only fails when the reporter evaluated both as non-pass.
        return self.dkim == "pass" or self.spf == "pass"


@dataclass
class DomainStats:
    """Everything seen for one header-from domain, across all reports."""

    passed: int = 0
    failed: int = 0
    failures: list[Record] = field(default_factory=list)
    passes: list[Record] = field(default_factory=list)
    policies: set[str] = field(default_factory=set)
    orgs: set[str] = field(default_factory=set)

    @property
    def total(self) -> int:
        return self.passed + self.failed


@dataclass
class Problems:
    """The items we skipped: how many, and the first few, already sanitized."""

    listed: list[str] = field(default_factory=list)
    total: int = 0

    def add(self, reason: str, label: str = "") -> None:
        self.total += 1
        if len(self.listed) < MAX_PROBLEMS_LISTED:
            # Sanitized as two pieces rather than one: a 600-character member
            # name would otherwise spend the whole budget and truncate away the
            # reason it is being reported for.
            text = one_line(reason)
            prefix = one_line(label)
            if prefix:
                text = f"{prefix}: {text}"
            self.listed.append(text)

    def absorb(self, other: Problems, label: str) -> None:
        """Fold another tally in, keeping its count but only its kept text."""
        for message in other.listed:
            self.add(f"skipped {message}", label)
        self.total += other.total - len(other.listed)


class ReadError(Exception):
    """A file we could not turn into DMARC XML."""


class SkipRecord(Exception):
    """A `<record>` we cannot count; the message says why."""


@dataclass
class Document:
    """One report pulled out of a file, or the reason we could not get it."""

    label: str
    data: bytes | None = None
    error: str | None = None


def reason(err: BaseException) -> str:
    """An exception's message, or its name when it carries none.

    `MemoryError` and friends stringify to nothing, which would otherwise
    render as a bare `not valid XML:` -- an error report that reports nothing.
    """
    return str(err) or type(err).__name__


def one_line(text: str) -> str:
    """Collapse text to a single printable line.

    Everything this touches is written by a stranger -- report fields by the
    reporter, archive member names by whoever built the archive -- and all of
    it reaches a terminal.  A newline lets them fabricate whole lines of this
    tool's own output, an ESC lets them repaint the screen, and a bidi or
    zero-width mark lets one domain render as another.  Member names are the
    sharper edge of the two: XML text at least has expat rejecting the raw
    control characters, and a name has nothing in front of it.

    `str.isprintable` is false for every one of those and true for the ASCII
    space, so one pass over it covers the lot.

    Length is capped for the same reason the character set is: a `<org_name>`
    of 200 MB is a few hundred KB of gzip, and it is echoed in the footer.  The
    slice comes first so the scan is bounded too.
    """
    trimmed = text[: MAX_TEXT + 1]
    collapsed = " ".join("".join(c if c.isprintable() else " " for c in trimmed).split())
    if len(collapsed) > MAX_TEXT:
        return collapsed[:MAX_TEXT].rstrip() + "..."
    return collapsed


def plural(count: int, noun: str) -> str:
    return f"{count:,} {noun}" if count == 1 else f"{count:,} {noun}s"


def is_tar(data: bytes) -> bool:
    return data[TAR_MAGIC_OFFSET : TAR_MAGIC_OFFSET + len(TAR_MAGIC)] == TAR_MAGIC


def gunzip(data: bytes) -> bytes:
    """Decompress, refusing to materialize more than `MAX_DECOMPRESSED`.

    The catch is deliberately broad.  Damaged input reaches the decompressors
    through several layers, and they signal it inconsistently -- a corrupt
    DEFLATE stream raises `zlib.error`, which descends straight from
    `Exception` and so slips past any list of the obvious I/O errors.  Getting
    that wrong turns one bad attachment into a traceback, and a traceback exits
    1: the same status a caller reads as "mail failed DMARC".
    """
    try:
        with gzip.GzipFile(fileobj=io.BytesIO(data)) as stream:
            out = stream.read(MAX_DECOMPRESSED + 1)
    # Running out of memory is not a damaged attachment, and swallowing
    # it here would drop the rest of this archive and let the summary
    # claim PASS over input it never read.  It belongs to `main`.
    except MemoryError:
        raise
    except Exception as err:
        raise ReadError(f"could not gunzip: {reason(err)}") from err
    if len(out) > MAX_DECOMPRESSED:
        raise ReadError(f"refusing to decompress more than {LIMIT_DESCRIPTION}")
    return out


def documents(label: str, data: bytes, depth: int = 0):
    """Yield a `Document` for every report reachable from `data`.

    Recursive because the envelopes nest: reporters send `.xml.gz` members
    inside a `.zip`, and dispatching on magic bytes at every level costs
    nothing and handles the combinations we would otherwise have to enumerate.

    Nothing in here raises.  A bad member is yielded as an error and the rest
    of the archive is still read: aborting would drop the reports sitting
    beside it, which could turn a run that contained failures into a clean
    `PASS` and an exit status to match.
    """
    # An all-NUL file is an empty tar (its end-of-archive blocks carry no
    # `ustar` magic), which would otherwise reach the XML parser and be
    # reported as a syntax error.
    if not data.lstrip(b"\0"):
        yield Document(label, error="file is empty")
        return

    if data[:2] == b"\x1f\x8b":
        try:
            data = gunzip(data)
        except ReadError as err:
            yield Document(label, error=str(err))
            return
        if not data:
            yield Document(label, error="file is empty once decompressed")
            return

    # Every zip variant starts "PK"; matching the local-file-header magic alone
    # would send an empty archive (which starts with the central directory) off
    # to the XML parser to fail with a confusing message.
    container = data[:2] == b"PK" or is_tar(data)
    if not container:
        yield Document(label, data=data)
    # `depth` counts containers already opened, so this is checked here rather
    # than on entry: a report that merely sits at the limit still gets read,
    # and only opening one container too many is refused.
    elif depth >= MAX_NESTING:
        yield Document(label, error="archives nested too deeply")
    elif data[:2] == b"PK":
        yield from zip_documents(label, data, depth)
    else:
        yield from tar_documents(label, data, depth)


def zip_entry_count(data: bytes) -> int | None:
    """How many entries the zip's own end-of-directory record claims.

    `zipfile` walks the central directory by accumulating record lengths until
    it has consumed the declared byte size, and never compares what it found
    against this count -- so corrupting one record's comment-length field
    silently truncates the listing, exactly as a damaged tar header silently
    ends the walk.  `unzip -t` rejects such a file; nothing in `zipfile` does.

    None when there is no count to trust: no end-of-directory record, or the
    zip64 sentinel, where the real count lives in a record we do not parse.
    """
    marker = data.rfind(b"PK\x05\x06")
    if marker < 0 or marker + 12 > len(data):
        return None
    count = int.from_bytes(data[marker + 10 : marker + 12], "little")
    return None if count == 0xFFFF else count


def zip_documents(label: str, data: bytes, depth: int):
    try:
        archive = zipfile.ZipFile(io.BytesIO(data))
    # Broad for the reason `gunzip` is: a mangled central directory surfaces as
    # anything from BadZipFile to UnicodeDecodeError on a member name.
    # Running out of memory is not a damaged attachment, and swallowing
    # it here would drop the rest of this archive and let the summary
    # claim PASS over input it never read.  It belongs to `main`.
    except MemoryError:
        raise
    except Exception as err:
        yield Document(label, error=f"could not unzip: {reason(err)}")
        return

    with archive:
        entries = archive.infolist()
        declared = zip_entry_count(data)
        damaged = declared is not None and declared != len(entries)
        # `is_dir` is only a trailing slash, so an entry named like a directory
        # but carrying a payload would be filtered away with the real ones.
        # Read it instead: content present in the file should never vanish
        # without a word, even if a conformant reader would discard it.
        members = [
            member
            for member in entries
            if not (member.is_dir() and not member.file_size)
        ]
        for member in members:
            name = f"{label}:{one_line(member.filename) or '(unnamed)'}"
            try:
                body = zip_member(archive, member)
            except ReadError as err:
                yield Document(name, error=str(err))
                continue
            yield from documents(name, body, depth + 1)
        if damaged:
            yield Document(label, error="zip archive is damaged or truncated")
        elif not members:
            yield Document(label, error="zip archive is empty")


def zip_member(archive: zipfile.ZipFile, member: zipfile.ZipInfo) -> bytes:
    if member.file_size > MAX_DECOMPRESSED:
        raise ReadError(f"member is larger than {LIMIT_DESCRIPTION}")
    try:
        with archive.open(member) as handle:
            body = handle.read(MAX_DECOMPRESSED + 1)
    # Unsupported compression and encrypted members surface as RuntimeError
    # subclasses, a corrupt DEFLATE stream as zlib.error, a mangled header as
    # ValueError or EOFError.  Every one of them means the same thing here.
    # Running out of memory is not a damaged attachment, and swallowing
    # it here would drop the rest of this archive and let the summary
    # claim PASS over input it never read.  It belongs to `main`.
    except MemoryError:
        raise
    except Exception as err:
        raise ReadError(f"could not read from the zip: {reason(err)}") from err
    if len(body) > MAX_DECOMPRESSED:
        raise ReadError(f"member is larger than {LIMIT_DESCRIPTION}")
    return body


def tar_documents(label: str, data: bytes, depth: int):
    try:
        archive = tarfile.open(fileobj=io.BytesIO(data))
    # As broad as the zip constructor above, and for the same reason: the
    # header parsers reach for codecs and integer conversions that do not
    # signal damage as a TarError.
    # Running out of memory is not a damaged attachment, and swallowing
    # it here would drop the rest of this archive and let the summary
    # claim PASS over input it never read.  It belongs to `main`.
    except MemoryError:
        raise
    except Exception as err:
        yield Document(label, error=f"could not untar: {reason(err)}")
        return

    with archive:
        # Walked one member at a time rather than through `getmembers`, which
        # builds the whole index and throws it all away if any header is
        # damaged: a tar truncated after two good reports would take both down
        # with it.  Here they are already yielded when the damage is reached.
        found = 0
        members = iter(archive)
        while True:
            try:
                member = next(members)
            except StopIteration:
                break
            # Running out of memory is not a damaged attachment, and swallowing
            # it here would drop the rest of this archive and let the summary
            # claim PASS over input it never read.  It belongs to `main`.
            except MemoryError:
                raise
            except Exception as err:
                yield Document(label, error=f"could not untar: {reason(err)}")
                return
            if not member.isfile():
                continue
            found += 1
            name = f"{label}:{one_line(member.name) or '(unnamed)'}"
            try:
                body = tar_member(archive, member)
            except ReadError as err:
                yield Document(name, error=str(err))
                continue
            if body is not None:
                yield from documents(name, body, depth + 1)
        if not found:
            yield Document(label, error="tar archive contains no files")
        elif archive.offset >= len(data) or data.count(
            b"\0", archive.offset
        ) != len(data) - archive.offset:
            # `TarFile.next` returns None rather than raising for a damaged
            # header anywhere but the first, so the loop above cannot tell a
            # mangled one from a clean end of archive.  A well-formed tar ends
            # in NUL padding, so anything else left over means members were
            # dropped -- and dropping them quietly is exactly how a run whose
            # failures sit in the tail comes out as `PASS`.  GNU tar says
            # "Skipping to next header" here; saying nothing is not an option.
            yield Document(label, error="tar archive is damaged or truncated")


def tar_member(archive: tarfile.TarFile, member: tarfile.TarInfo) -> bytes | None:
    if member.size > MAX_DECOMPRESSED:
        raise ReadError(f"member is larger than {LIMIT_DESCRIPTION}")
    try:
        # `extractfile` reads the member out of the stream; nothing is ever
        # written to disk, so member paths cannot escape anywhere.
        handle = archive.extractfile(member)
        if handle is None:
            return None
        with handle:
            return handle.read()
    # Running out of memory is not a damaged attachment, and swallowing
    # it here would drop the rest of this archive and let the summary
    # claim PASS over input it never read.  It belongs to `main`.
    except MemoryError:
        raise
    except Exception as err:
        raise ReadError(f"could not read from the tar: {reason(err)}") from err


def read(path: Path):
    # Opened non-blocking and then judged through the descriptor.  Plain `open`
    # on a FIFO waits for a writer forever, and testing the path first would
    # answer for whatever the name meant a moment ago rather than for the thing
    # actually opened.  O_NONBLOCK means nothing to a regular file.
    try:
        handle = os.fdopen(os.open(path, os.O_RDONLY | os.O_NONBLOCK), "rb")
    except OSError as err:
        yield Document(str(path), error=err.strerror or reason(err))
        return

    try:
        with handle:
            info = os.fstat(handle.fileno())
            if not stat.S_ISREG(info.st_mode):
                yield Document(str(path), error="not a regular file")
                return
            # `st_size` is the cheap early out; the bounded read is the one
            # that actually holds, since a file can report zero (anything under
            # /proc) or grow after the stat (a report still being written).
            if info.st_size > MAX_DECOMPRESSED:
                yield Document(
                    str(path), error=f"file is larger than {LIMIT_DESCRIPTION}"
                )
                return
            data = handle.read(MAX_DECOMPRESSED + 1)
            if len(data) > MAX_DECOMPRESSED:
                yield Document(
                    str(path), error=f"file is larger than {LIMIT_DESCRIPTION}"
                )
                return
    except OSError as err:
        yield Document(str(path), error=err.strerror or reason(err))
        return

    yield from documents(str(path), data)


def child_text(node, path: str, default: str = "") -> str:
    if node is None:
        return default
    found = node.find(path)
    if found is None or found.text is None:
        return default
    # `or default` because an element holding only whitespace is not a value:
    # pretty-printed reports are full of them, and without this they render as
    # a bare `p=` or a reporter with no name.
    return one_line(found.text) or default


def child_int(node, path: str) -> int | None:
    raw = child_text(node, path)
    try:
        return int(raw)
    except ValueError:
        return None


def timestamp(node, path: str) -> int | None:
    """Read a report timestamp, discarding values `datetime` cannot hold.

    Some reporters emit milliseconds where the schema says seconds, and an
    unguarded `fromtimestamp` on one of those raises out of the middle of an
    otherwise complete run.
    """
    value = child_int(node, path)
    if value is None or not MIN_TIMESTAMP <= value <= MAX_TIMESTAMP:
        return None
    return value


def strip_namespaces(root) -> None:
    """Drop XML namespaces so plain tag names match everywhere.

    The DMARC schema is namespace-free, but a few reporters emit one anyway and
    every `find` below would silently miss if we left it on.
    """
    for element in root.iter():
        if isinstance(element.tag, str) and "}" in element.tag:
            element.tag = element.tag.rsplit("}", 1)[1]


def parse(data: bytes) -> tuple[Report, list[Record], Problems]:
    """Return the report, its usable records, and a tally of the unusable."""
    try:
        root = ElementTree.fromstring(data)
    # Not just ParseError: an XML declaration naming an encoding Python has no
    # codec for raises LookupError from well below the parser.
    # Running out of memory is not a damaged attachment, and swallowing
    # it here would drop the rest of this archive and let the summary
    # claim PASS over input it never read.  It belongs to `main`.
    except MemoryError:
        raise
    except Exception as err:
        raise ReadError(f"not valid XML: {reason(err)}") from err

    strip_namespaces(root)
    if root.tag != "feedback":
        raise ReadError(f"not a DMARC report (root element is <{root.tag}>)")

    metadata = root.find("report_metadata")
    published = root.find("policy_published")
    date_range = metadata.find("date_range") if metadata is not None else None
    report = Report(
        org=child_text(metadata, "org_name", "unknown"),
        # DNS names are case-insensitive and reporters echo whatever casing the
        # header carried, so fold it here: otherwise one domain shows up as two.
        domain=child_text(published, "domain").lower(),
        # Folded like every other verdict field: two reporters spelling the
        # same policy `Reject` and `reject` would otherwise render as two.
        policy=child_text(published, "p", "unknown").lower(),
        subdomain_policy=child_text(published, "sp").lower(),
        begin=timestamp(date_range, "begin"),
        end=timestamp(date_range, "end"),
    )

    records = []
    skipped = Problems()
    for node in root.findall("record"):
        try:
            record = parse_record(report, node)
        except SkipRecord as err:
            skipped.add(str(err))
            continue
        if record is not None:
            records.append(record)
    return report, records, skipped


def published_policy(report: Report, domain: str) -> str:
    """The policy that governed this domain, tagged with where it came from.

    RFC 7489 section 6.3: `sp`, when published, applies to subdomains in place
    of `p`.  Reporting `p` for a subdomain would contradict the disposition
    printed right below it whenever the two differ -- and printing the `sp`
    value under a `p=` label would assert something the report never said, so
    the tag travels with the value.
    """
    if (
        report.subdomain_policy
        and report.domain
        and domain != report.domain
        and domain.endswith(f".{report.domain}")
    ):
        return f"sp={report.subdomain_policy}"
    return f"p={report.policy}"


def parse_record(report: Report, node) -> Record | None:
    row = node.find("row")
    evaluated = row.find("policy_evaluated") if row is not None else None
    auth = node.find("auth_results")
    identifiers = node.find("identifiers")

    count = child_int(row, "count")
    if count is None:
        raise SkipRecord(f"record with unreadable <count> {child_text(row, 'count')!r}")
    if count < 0:
        raise SkipRecord(f"record with negative <count> {count}")
    if count == 0:
        # Legitimate and empty: nothing to summarize either way.
        return None

    dkim_auth = [
        (
            child_text(entry, "domain").lower(),
            child_text(entry, "selector"),
            child_text(entry, "result", "unknown").lower(),
        )
        for entry in (auth.findall("dkim") if auth is not None else [])
    ]
    spf_auth = [
        (
            child_text(entry, "domain").lower(),
            child_text(entry, "result", "unknown").lower(),
        )
        for entry in (auth.findall("spf") if auth is not None else [])
    ]
    overrides = [
        (child_text(entry, "type", "unknown"), child_text(entry, "comment"))
        for entry in (evaluated.findall("reason") if evaluated is not None else [])
    ]

    domain = child_text(identifiers, "header_from").lower() or report.domain
    return Record(
        domain=domain or "(unknown)",
        policy=published_policy(report, domain),
        ip=child_text(row, "source_ip", "(unknown)"),
        count=count,
        disposition=child_text(evaluated, "disposition", "none").lower(),
        dkim=child_text(evaluated, "dkim", "unknown").lower(),
        spf=child_text(evaluated, "spf", "unknown").lower(),
        dkim_auth=dkim_auth,
        spf_auth=spf_auth,
        overrides=overrides,
    )


def merge(records: list[Record]) -> list[Record]:
    """Collapse records that say the same thing so counts read as one line.

    Reporters split a single sender across many `<record>`s -- one per report,
    sometimes one per hour -- and listing each separately buries the signal.

    The key deliberately omits `domain`, so every caller must pass records for
    a single domain: mixing them would sum counts across domains and label the
    result with whichever one happened to be seen first.  Auth results are
    sorted into the key because two reporters describing the same pair of DKIM
    signatures need not list them in the same order.
    """
    merged: dict[tuple, Record] = {}
    for record in records:
        key = (
            record.ip,
            record.disposition,
            record.dkim,
            record.spf,
            tuple(sorted(record.dkim_auth)),
            tuple(sorted(record.spf_auth)),
            tuple(sorted(record.overrides)),
        )
        if key in merged:
            merged[key].count += record.count
        else:
            merged[key] = Record(**vars(record))
    return sorted(merged.values(), key=lambda r: (-r.count, r.ip))


def describe_dkim(record: Record) -> str:
    """Say why DMARC's DKIM leg came out the way it did."""
    if record.dkim == "pass":
        return "passed and aligned"

    passing = sorted(
        {domain or "(no domain)" for domain, _, result in record.dkim_auth if result == "pass"}
    )
    if passing:
        return f"signature valid for {', '.join(passing)} -- not aligned with {record.domain}"
    if not record.dkim_auth:
        return "message was not signed"

    parts = []
    for domain, selector, result in record.dkim_auth:
        name = domain or "(no domain)"
        if selector:
            name = f"{name} (selector {selector})"
        parts.append(f"{result} for {name}")
    return "; ".join(parts)


def describe_spf(record: Record) -> str:
    """Say why DMARC's SPF leg came out the way it did."""
    if record.spf == "pass":
        return "passed and aligned"

    passing = sorted(
        {domain or "(no domain)" for domain, result in record.spf_auth if result == "pass"}
    )
    if passing:
        return f"passed for {', '.join(passing)} -- not aligned with {record.domain}"
    if not record.spf_auth:
        return "no SPF result reported"

    return "; ".join(
        f"{result} for {domain or '(no domain)'}" for domain, result in record.spf_auth
    )


def describe_disposition(record: Record) -> str:
    action = {
        "none": "delivered",
        "quarantine": "quarantined",
        "reject": "rejected",
    }.get(record.disposition, record.disposition)
    if record.overrides:
        reasons = ", ".join(
            f"{kind}{f': {comment}' if comment else ''}" for kind, comment in record.overrides
        )
        return f"{action} (policy override -- {reasons})"
    return action


def collect(paths: list[Path]) -> tuple[dict[str, DomainStats], list[Report], Problems]:
    domains: dict[str, DomainStats] = defaultdict(DomainStats)
    reports: list[Report] = []
    problems = Problems()
    # `expand` needs `problems` because a directory it cannot walk is itself
    # something skipped, and nothing above this call would ever hear about it.

    for path in expand(paths, problems):
        # Streamed, not collected: members are only bounded one at a time, so
        # holding a whole path's worth at once turns a small attachment of many
        # well-compressed members into gigabytes.  Guarding `next` alone keeps
        # the backstop without giving that up -- every layer below reports its
        # own failures as `Document`s, so reaching this catch means a bug here
        # rather than a bad attachment, and it should cost one path and say so
        # rather than exit with a traceback that reads as a DMARC failure.
        stream = iter(read(path))
        while True:
            try:
                document = next(stream)
            except StopIteration:
                break
            # Running out of memory is not a damaged attachment, and swallowing
            # it here would drop the rest of this archive and let the summary
            # claim PASS over input it never read.  It belongs to `main`.
            except MemoryError:
                raise
            except Exception as err:  # pragma: no cover - defensive
                problems.add(f"internal error: {err!r}", str(path))
                break

            if document.data is None:
                problems.add(document.error or "unreadable", document.label)
                continue
            try:
                report, records, skipped = parse(document.data)
            except ReadError as err:
                problems.add(str(err), document.label)
                continue
            problems.absorb(skipped, document.label)
            reports.append(report)
            for record in records:
                stats = domains[record.domain]
                stats.policies.add(record.policy)
                stats.orgs.add(report.org.lower())
                if record.passed:
                    stats.passed += record.count
                    stats.passes.append(record)
                else:
                    stats.failed += record.count
                    stats.failures.append(record)

    return domains, reports, problems


def format_date_range(reports: list[Report]) -> str:
    stamps = [
        stamp
        for report in reports
        for stamp in (report.begin, report.end)
        if stamp is not None
    ]
    if not stamps:
        return ""
    start = datetime.fromtimestamp(min(stamps), timezone.utc)
    end = datetime.fromtimestamp(max(stamps), timezone.utc)
    if start.date() == end.date():
        return start.strftime("%Y-%m-%d")
    return f"{start:%Y-%m-%d} to {end:%Y-%m-%d}"


def unique_orgs(reports: list[Report]) -> list[str]:
    """Reporter names, deduplicated case-insensitively, first spelling kept."""
    seen: dict[str, str] = {}
    for report in reports:
        seen.setdefault(report.org.lower(), report.org)
    return sorted(seen.values(), key=str.lower)


def render_sources(records: list[Record], style: Style, color: str) -> list[str]:
    """One indented block per sending IP, with the auth verdicts under it."""
    lines = []
    merged = merge(records)
    width = max(len(f"{record.count:,}") for record in merged)
    ip_width = max(len(record.ip) for record in merged)
    for record in merged:
        count = style(f"{record.count:>{width},}", "bold")
        disposition = style(f"-> {describe_disposition(record)}", "dim")
        lines.append(f"    {count}  {record.ip:<{ip_width}}  {disposition}")
        for name, result, detail in (
            ("SPF ", record.spf, describe_spf(record)),
            ("DKIM", record.dkim, describe_dkim(record)),
        ):
            tint = "green" if result == "pass" else color
            # Pad before coloring: the escape codes would otherwise count
            # toward the field width and knock the column out of alignment.
            # Nine, not eight: `temperror` and `permerror` are out of schema
            # for `policy_evaluated` but reporters do emit them, and a value
            # that overflows the field shunts the detail column along with it.
            verdict = style(f"{result:<9}", tint)
            lines.append(f"    {' ' * width}  {style(name, 'dim')} {verdict} {detail}")
    return lines


def render_failures(domains, style: Style, show_all: bool) -> list[str]:
    out: list[str] = []
    for domain, stats in domains:
        out.append("")
        policies = ", ".join(sorted(stats.policies))
        headline = f"  {style(domain, 'bold')}  {stats.failed:,} of {stats.total:,} failed"
        out.append(f"{headline}  {style(f'(published policy: {policies})', 'dim')}")
        out.extend(render_sources(stats.failures, style, "red"))
        if show_all and stats.passes:
            out.append(f"    {style('and passing:', 'dim')}")
            out.extend(render_sources(stats.passes, style, "yellow"))
    return out


def render_clean(domains, style: Style, show_all: bool) -> list[str]:
    out = ["", style("Passed all checks", "dim")]
    width = max(len(domain) for domain, _ in domains)
    for domain, stats in domains:
        summary = f"{plural(stats.total, 'message')} from {plural(len(stats.orgs), 'reporter')}"
        out.append(style(f"  {domain:<{width}}  {summary}", "dim"))
        if show_all and stats.passes:
            out.extend(render_sources(stats.passes, style, "yellow"))
    return out


def render(
    domains: dict[str, DomainStats],
    reports: list[Report],
    problems: Problems,
    style: Style,
    show_all: bool,
) -> tuple[str, int]:
    failed = sum(stats.failed for stats in domains.values())
    passed = sum(stats.passed for stats in domains.values())
    total = failed + passed
    failing = sorted(
        (item for item in domains.items() if item[1].failed),
        key=lambda item: -item[1].failed,
    )
    clean = sorted(
        (item for item in domains.items() if not item[1].failed),
        key=lambda item: -item[1].total,
    )

    out: list[str] = []
    if not total:
        out.append(style("No DMARC records found.", "bold", "yellow"))
    elif failed:
        # A single failure in a large sample rounds to 0.0%, which reads as
        # nothing wrong; the counts either side of it carry the real weight.
        share = f"{failed / total:.1%}"
        out.append(
            style(f"FAIL  {failed:,} of {total:,} messages failed DMARC ({share})", "bold", "red")
        )
    else:
        headline = f"PASS  all {total:,} messages passed DMARC"
        if problems.total:
            # Not an unqualified all-clear.  Something was not read and what it
            # held is unknown, so the caveat belongs on the headline rather than
            # only in the list below: that line is the whole answer for anyone
            # skimming, and "all passed" over input we skipped is a claim this
            # run cannot support.
            headline += f"  ({plural(problems.total, 'item')} skipped, see below)"
        out.append(style(headline, "bold", "green"))

    out.extend(render_failures(failing, style, show_all))
    if clean:
        out.extend(render_clean(clean, style, show_all))

    if reports:
        out.append("")
        footer = f"{plural(len(reports), 'report')} from {', '.join(unique_orgs(reports))}"
        window = format_date_range(reports)
        if window:
            footer += f", {window}"
        out.append(style(footer, "dim"))

    if problems.total:
        out.append("")
        out.append(style(f"Skipped {plural(problems.total, 'unreadable item')}:", "yellow"))
        out.extend(f"  {problem}" for problem in problems.listed)
        if problems.total > len(problems.listed):
            hidden = problems.total - len(problems.listed)
            out.append(style(f"  ... and {hidden:,} more", "dim"))

    # An unreadable stray file does not change the verdict on the mail that did
    # parse -- `expand` walks directories, so a README in one is routine.  The
    # test for 2 is whether a report was *read*, not whether it carried
    # countable records: a report of all-zero counts was read fine.
    if failed:
        code = 1
    elif not reports:
        code = 2
    else:
        code = 0
    return "\n".join(out), code


def expand(paths: list[Path], problems: Problems) -> list[Path]:
    """Walk directory arguments so a whole folder of reports can be passed.

    Not `Path.rglob`, which loses entries two ways and says nothing about
    either: it swallows `PermissionError` on a directory it cannot open, and
    it refuses to descend into a symlinked one.  A mail archive is exactly
    where both turn up -- reports delivered under another uid, a `2025-08`
    directory linked in from elsewhere -- and losing them quietly is how a run
    whose failures live down that branch comes out `PASS`, which is the same
    silence a damaged archive used to get away with.

    So the walk is explicit: every directory it cannot read is reported, and
    symlinks are followed with the inodes already seen held against loops.
    Whatever it yields that is not a readable file -- a dangling link, a FIFO
    -- `read` names, rather than the walk dropping it.
    """
    expanded: list[Path] = []
    for path in paths:
        try:
            directory = path.is_dir()
        except OSError:
            # Even asking can fail, on a name past NAME_MAX; let `read` say so.
            expanded.append(path)
            continue
        if not directory:
            expanded.append(path)
            continue

        def note(err: OSError, root: Path = path) -> None:
            problems.add(err.strerror or reason(err), str(err.filename or root))

        seen: set[tuple[int, int]] = set()
        found: list[Path] = []
        for parent, subdirectories, names in os.walk(path, onerror=note, followlinks=True):
            try:
                info = os.stat(parent)
            except OSError as err:
                note(err)
                subdirectories[:] = []
                continue
            marker = (info.st_dev, info.st_ino)
            if marker in seen:
                subdirectories[:] = []
                continue
            seen.add(marker)
            found.extend(Path(parent) / name for name in names)
        expanded.extend(sorted(found))
    return expanded


def stdout_is_terminal() -> bool:
    """Whether stdout is a terminal, for a stdout that may not be there at all.

    Started with fd 1 closed, `sys.stdout` is None; on an already-closed stream
    `isatty` raises outright.  Neither is a reason to fail the run, and both
    mean the same thing here -- nobody is watching, so do not colorize.
    """
    try:
        return sys.stdout is not None and sys.stdout.isatty()
    except (AttributeError, ValueError):
        return False


def silence_stdout() -> None:
    """Point stdout at the void so the interpreter's exit flush stays quiet."""
    try:
        devnull = os.open(os.devnull, os.O_WRONLY)
    except OSError:
        return
    try:
        os.dup2(devnull, sys.stdout.fileno())
    except (AttributeError, OSError, ValueError):
        pass
    finally:
        os.close(devnull)


def warn(message: str) -> None:
    """Best-effort note on stderr, which may be no better off than stdout.

    `MemoryError` is caught here along with the rest, because this is called
    from the handler for exactly that -- with the process already at its
    ceiling, where formatting a string is itself an allocation.  An exception
    escaping from here would be a traceback and exit 1: the status meaning the
    mail failed, which is the one thing that must never happen by accident.
    """
    stream = sys.stderr
    if stream is None:
        # `print(file=None)` falls through to stdout, which every caller here
        # has just pointed at the void -- so this would be silently dropped
        # anyway, and saying so is clearer than relying on that.
        return
    try:
        print(f"read-dmarc: {message}", file=stream)
    except (AttributeError, MemoryError, OSError, UnicodeEncodeError, ValueError):
        pass


def write_summary(text: str) -> str | None:
    """Print the summary.  None if it went out, else why it could not.

    Only `BrokenPipeError` means "the reader stopped", which says nothing
    about the mail.  Everything else here -- a full disk, an fd opened
    read-only, an stdout encoding too narrow for a reporter's domain name --
    is a failure to deliver the answer, and must not come back as 1, the
    status that means the mail failed.
    """
    stream = sys.stdout
    if stream is None:
        # Started with fd 1 closed: nowhere to write, and nobody to tell.
        return None

    try:
        stream.write(f"{text}\n")
        stream.flush()
        return None
    except BrokenPipeError:
        # `| head`, or quitting a pager early.  Routine.
        silence_stdout()
        return None
    except UnicodeEncodeError:
        pass
    except (OSError, ValueError) as err:
        silence_stdout()
        return str(err)

    # An stdout too narrow to hold a reporter's domain name -- an ASCII locale,
    # say -- should still get the verdict, with the few characters it cannot
    # represent replaced rather than the whole summary withheld.
    try:
        encoding = getattr(stream, "encoding", None) or "ascii"
        stream.buffer.write(f"{text}\n".encode(encoding, "replace"))
        stream.flush()
        return None
    except BrokenPipeError:
        # As above, and for the same reason: the reader stopped.  Without this
        # clause the `OSError` below would swallow it and report a write
        # failure for what the direct path calls routine.
        silence_stdout()
        return None
    except (AttributeError, LookupError, OSError, UnicodeEncodeError, ValueError) as err:
        silence_stdout()
        return str(err)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="read-dmarc",
        description="Summarize DMARC aggregate reports: what failed, and how.",
        epilog=(
            "exit status: 0 everything passed, 1 some mail failed DMARC, "
            "2 no report could be read, 3 the summary could not be written"
        ),
    )
    parser.add_argument(
        "files",
        metavar="FILE",
        nargs="+",
        type=Path,
        help="DMARC reports as .xml, .zip, .gz, .tar or .tar.gz; directories are walked",
    )
    parser.add_argument(
        "-a",
        "--all",
        action="store_true",
        help="also break passing mail down by sending IP",
    )
    parser.add_argument(
        "--color",
        choices=("auto", "always", "never"),
        default="auto",
        help="colorize the output (default: auto, meaning only on a terminal)",
    )
    args = parser.parse_args(argv)

    style = Style(args.color == "always" or (args.color == "auto" and stdout_is_terminal()))
    try:
        domains, reports, problems = collect(args.files)
        text, code = render(domains, reports, problems, style, args.all)
        # Writing sits inside the same guard: `write_summary` doubles the
        # summary in memory, and formatting the message below allocates too,
        # so both can fail exactly where the failure is hardest to survive.
        unwritten = write_summary(text)
        if unwritten is not None:
            warn(f"could not write the summary: {unwritten}")
            return 3
    except MemoryError:
        # The cap above bounds decompressed bytes, not the object graph they
        # parse into -- an order of magnitude more -- so a crafted report can
        # still exhaust a process under RLIMIT_AS.  A named failure beats a
        # traceback, which would exit 1: the status meaning the mail failed.
        warn("ran out of memory")
        return 3
    return code


if __name__ == "__main__":
    sys.exit(main())
