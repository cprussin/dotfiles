#!/usr/bin/env python3
"""Apply byte-length-preserving source patches inside app.asar.

Vendored from numtide/llm-agents.nix, packages/chatgpt/patch-asar.py, taken
from main on 2026-08-28; diff against that path to re-sync when a version bump
moves the minified code these patterns match.  Local changes since: the
executor-plugins patch and the pre-copy chmod in COPY_PLUGINS_WRITABLE.

Copyright (c) 2024 Numtide

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

The asar header records file offsets, so every replacement is padded with
spaces to the original's exact byte length instead of re-packing the archive.
Minified identifiers change between releases, so patterns are regexes that
capture the identifiers they need instead of hard-coding them.
"""

import re
import sys
from collections.abc import Callable
from pathlib import Path

# @parcel/watcher uses detect-libc in a named worker. Its process.report
# fallback trips a CFI guard in the bundled Owl/Electron runtime on NixOS.
# detect-libc falls back to its ELF/filesystem/ldd probes instead.
SKIP_PROCESS_REPORT = (
    re.compile(rb"isLinux\(\) && process\.report"),
    lambda _m: b"false /* nix:skip report */",
)

# The app materializes bundled plugins in ~/.codex and rewrites selected
# manifests there. Node's fs.cp preserves the Nix store's read-only modes,
# so copy with coreutils and make only the user-owned destination writable.
# `exec` is the promisified execFile helper already used for the darwin
# `ditto` branch. The leading chmod (which fails harmlessly when the
# destination is new) lets the copy overwrite a tree an unpatched fs.cp
# already left read-only. Fitting all three commands in the original's bytes
# means one `sh -c` and dropping the darwin branch, dead in a Linux build.
COPY_PLUGINS_WRITABLE = (
    re.compile(
        rb"(?P<fn>async function [\w$]+\(e,t\)\{)"
        rb"if\((?P<plat>[\w$]+\.default\.platform)===`darwin`\)"
        rb"\{await (?P<exec>[\w$]+)\(`/usr/bin/ditto`,\[`--noqtn`,e,t\]\);return\}"
        rb"if\((?P=plat)!==`win32`\)\{"
        rb"await [\w$]+\.default\.cp\(e,t,\{recursive:!0,verbatimSymlinks:!0\}\);return\}"
    ),
    lambda m: (
        m["fn"]
        + b"if("
        + m["plat"]
        + b"!==`win32`){await "
        + m["exec"]
        + b"(`/bin/sh`,[`-c`,`chmod -Rf u+w \"$1\";cp -r \"$0\"/. \"$1\"&&chmod -R u+w \"$1\"`,e,t]);return}"
    ),
)


def _copy_plugins_fn(data: bytes) -> bytes:
    """Name of the function COPY_PLUGINS_WRITABLE patches."""
    m = re.search(COPY_PLUGINS_WRITABLE[0], data)
    if m is None:
        sys.exit("COPY_PLUGINS_WRITABLE target not found")
    return re.search(rb"async function ([\w$]+)", m["fn"])[1]


# The app also copies its bundled codex-app-tools MCP server into
# ~/.config/Codex/executor-plugins with a bare fs.cp, then writes .mcp.json
# beside it: the same read-only-store problem, failing the executor startup.
# Route it through the helper COPY_PLUGINS_WRITABLE already fixes.
COPY_EXECUTOR_PLUGINS_WRITABLE = (
    re.compile(
        rb"(?P<head>async function [\w$]+\(\{executorPluginRoot:(?P<dst>[\w$]+),"
        rb"[^)]*\)\{.{0,160}?)"
        rb"await [\w$]+\.default\.cp\((?P<src>[\w$]+\.cwd),(?P=dst),\{recursive:!0\}\)"
    ),
    lambda m, data: (
        m["head"]
        + b"await "
        + _copy_plugins_fn(data)
        + b"("
        + m["src"]
        + b","
        + m["dst"]
        + b")"
    ),
)

PATCHES: list[
    tuple[re.Pattern[bytes], Callable[[re.Match[bytes], bytes], bytes]]
] = [
    (SKIP_PROCESS_REPORT[0], lambda m, _data: SKIP_PROCESS_REPORT[1](m)),
    # Before COPY_PLUGINS_WRITABLE, which it finds its helper's name by.
    COPY_EXECUTOR_PLUGINS_WRITABLE,
    (COPY_PLUGINS_WRITABLE[0], lambda m, _data: COPY_PLUGINS_WRITABLE[1](m)),
]


def main() -> None:
    """Patch the asar archive given as the only argument."""
    asar = Path(sys.argv[1])
    data = asar.read_bytes()
    for pattern, build in PATCHES:
        matches = list(pattern.finditer(data))
        if len(matches) != 1:
            sys.exit(
                f"expected 1 match for {pattern.pattern[:60]!r} in {asar}, got {len(matches)}"
            )
        m = matches[0]
        original = m.group(0)
        replacement = build(m, data)
        if len(replacement) > len(original):
            sys.exit(f"replacement longer than original: {replacement[:60]!r}...")
        data = (
            data[: m.start()] + replacement.ljust(len(original), b" ") + data[m.end() :]
        )
    asar.write_bytes(data)


if __name__ == "__main__":
    main()
