#!/usr/bin/env bash
# What bootstrap-chromium-tree.sh decides, against a /build made of fixtures.
#
#   config/machines/crux/test-bootstrap-chromium-tree.sh [path to the script]
#
# The sync itself is not exercised and cannot be: it is hours of
# chromium.googlesource.com. What is exercised is every decision around it --
# which slot, whether there is room, and what is left behind when the sync
# fails, which is the one this design turns on.
set -u

SCRIPT="${1:-$(dirname "$0")/bootstrap-chromium-tree.sh}"
[ -f "$SCRIPT" ] || { echo "no script at $SCRIPT" >&2; exit 2; }
# Absolute, because every case runs it from inside its own fixture.
SCRIPT="$(cd "$(dirname "$SCRIPT")" && pwd)/$(basename "$SCRIPT")"

pass=0
fail=0

check() { # name wanted got
  if printf '%s' "$3" | grep -qF -- "$2"; then
    echo "  ok   $1"
    pass=$((pass + 1))
  else
    echo "  FAIL $1"
    echo "    wanted: $2"
    echo "    got:    $3"
    fail=$((fail + 1))
  fi
}

# Exit codes are compared rather than searched for: `grep -F 1` matches 127 and
# 21 too, which would make every "fails" assertion pass for a script that was
# not found.
is() { # name wanted got
  if [ "$3" = "$2" ]; then
    echo "  ok   $1"
    pass=$((pass + 1))
  else
    echo "  FAIL $1"
    echo "    wanted: $2"
    echo "    got:    $3"
    fail=$((fail + 1))
  fi
}

# For the cases where the script aborts under `set -e` and so exits with
# whatever the failing command returned, rather than a code of its own.
nonzero() { # name got
  if [ "$2" != "0" ]; then
    echo "  ok   $1"
    pass=$((pass + 1))
  else
    echo "  FAIL $1"
    echo "    wanted: any non-zero exit"
    echo "    got:    $2"
    fail=$((fail + 1))
  fi
}

exists() { [ -e "$1" ] && echo yes || echo no; }

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

ROOM=$((200 * 1024 * 1024 * 1024))

# A /build with the slots named, and stubs for the two commands that would
# otherwise want a real machine.
fixture() { # name free-bytes gclient-body -- echoes the root
  local root="$TMP/$1"
  mkdir -p "$root/bin" "$root/depot_tools"
  printf '#!/bin/sh\necho %s\n' "$2" >"$root/bin/zfs"
  printf '#!/bin/sh\n%s\n' "$3" >"$root/bin/gclient"
  chmod +x "$root/bin/zfs" "$root/bin/gclient"
  cp "$root/bin/gclient" "$root/depot_tools/gclient"
  chmod +x "$root/depot_tools/gclient"
  : >"$root/depot_tools/python3_bin_reldir.txt"
  printf '%s' "$root"
}

run() { # root
  ( cd "$1" && BUILD_ROOT="$1" POOL=tank-fast PATH="$1/bin:$PATH" bash "$SCRIPT" 2>&1 )
}

echo "a slot with no checkout is the one that gets filled"
r="$(fixture fills $ROOM 'mkdir -p src; echo synced')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1"
out="$(run "$r")"
check "says which slot it filled"   "filled $r/trees/tree-1"  "$out"
check "the slot holds a checkout"   "yes"  "$(exists "$r/trees/tree-1/src")"
check "the populated slot is left"  "yes"  "$(exists "$r/trees/tree-0/src")"
check "nothing is left staged"      "no"   "$(exists "$r/bootstrap/tree-1")"
check "the solution is not managed" '"managed": False' "$(cat "$r/trees/tree-1/.gclient")"
# The pool reads a stamp-less slot as carrying no pin, which is what makes it
# the one the next repin takes rather than tree-0.
check "no pin is claimed for it"    "no"   "$(exists "$r/trees/tree-1/.domicile-synced-pin")"

echo "a full pool is nothing to do rather than something to fix"
r="$(fixture full $ROOM 'echo RAN-THE-SYNC')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1/src"
out="$(run "$r")"
rc=$?
check "says so"           "every slot under $r/trees holds a checkout" "$out"
is "succeeds"             "0" "$rc"
check "syncs nothing"     "no" "$(printf '%s' "$out" | grep -q RAN-THE-SYNC && echo yes || echo no)"

echo "a dataset without room for a tree is refused before the sync starts"
r="$(fixture cramped $((50 * 1024 * 1024 * 1024)) 'echo RAN-THE-SYNC')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1"
out="$(run "$r")"
rc=$?
check "names what is free" "has 50G free" "$out"
check "names what a tree costs" "a tree is 97G" "$out"
is "fails"                  "1" "$rc"
check "syncs nothing"       "no" "$(printf '%s' "$out" | grep -q RAN-THE-SYNC && echo yes || echo no)"

# THE ONE THIS DESIGN EXISTS FOR. engine-tree-pool.sh takes any slot holding a
# src/, and prefers one carrying no pin -- so a half-synced tree published into
# the pool would be picked by the very next engine run and built in.
echo "a sync that dies publishes nothing"
r="$(fixture died $ROOM 'mkdir -p src; echo "the network went away" >&2; exit 1')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1"
out="$(run "$r")"
rc=$?
is "fails"                           "1" "$rc"
check "the slot is still empty"      "no"  "$(exists "$r/trees/tree-1/src")"
check "the slot is still a slot"     "yes" "$(exists "$r/trees/tree-1")"
check "what it got is left on disk"  "yes" "$(exists "$r/bootstrap/tree-1/src")"

echo "an unbootstrapped depot_tools is refused by name"
r="$(fixture nodepot $ROOM 'echo RAN-THE-SYNC')"
mkdir -p "$r/trees/tree-1"
rm -f "$r/depot_tools/python3_bin_reldir.txt"
out="$(run "$r")"
rc=$?
check "names it"  "no bootstrapped depot_tools at $r/depot_tools" "$out"
is "fails"        "1" "$rc"

echo "no pool at all is a failure rather than a quiet success"
r="$(fixture nopool $ROOM 'echo RAN-THE-SYNC')"
out="$(run "$r")"
rc=$?
check "names the unit that makes it" "setup-chromium-trees.service has not run" "$out"
is "fails"                           "1" "$rc"


# The slot is filled by renaming a staged tree onto it, which needs it empty --
# so a slot holding anything else must be refused BEFORE the sync, not found
# out at the rename after it.
echo "a slot with no checkout but something else in it is refused before the sync"
r="$(fixture leftovers $ROOM 'echo RAN-THE-SYNC')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1"
printf 'abc123\n' >"$r/trees/tree-1/.domicile-synced-pin"
out="$(run "$r")"
rc=$?
check "says it is not empty" "holds no checkout, but it is not empty" "$out"
check "names what is in it"  ".domicile-synced-pin"                   "$out"
is    "fails"                "1"                                      "$rc"
check "syncs nothing"        "no" "$(printf '%s' "$out" | grep -q RAN-THE-SYNC && echo yes || echo no)"

# `[ x -lt y ]` exits 2 on a non-number and an `if` suspends `set -e`, so the
# guard would read "not less than" and start the sync.
echo "a zfs that answers with something that is not a number is not room"
r="$(fixture notanumber 0 'echo RAN-THE-SYNC')"
printf '#!/bin/sh\necho -\n' >"$r/bin/zfs"
chmod +x "$r/bin/zfs"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1"
out="$(run "$r")"
rc=$?
check "quotes what it was told" 'zfs answered "-"' "$out"
is    "fails"                   "1"                "$rc"
check "syncs nothing"           "no" "$(printf '%s' "$out" | grep -q RAN-THE-SYNC && echo yes || echo no)"

echo "a trees directory with no slots in it is not a full pool"
r="$(fixture noslots $ROOM 'echo RAN-THE-SYNC')"
mkdir -p "$r/trees"
out="$(run "$r")"
rc=$?
check "says there are no slots" "holds no slots" "$out"
is    "fails"                   "1"              "$rc"

echo "one slot per firing, and the first empty one"
r="$(fixture several $ROOM 'mkdir -p src; echo synced')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1" "$r/trees/tree-2"
out="$(run "$r")"
check "takes the first empty slot" "filled $r/trees/tree-1" "$out"
check "fills it"                   "yes" "$(exists "$r/trees/tree-1/src")"
check "leaves the second for tomorrow" "no" "$(exists "$r/trees/tree-2/src")"

# The toolchain downloads are hooks and belong inside upstream's FHS shell, and
# a --revision here would claim a pin the slot must not claim.
echo "the sync asks for no hooks and no revision"
r="$(fixture args $ROOM 'echo "ARGS: $*" >args.seen; mkdir -p src')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1"
run "$r" >/dev/null
check "passes --nohooks" "ARGS: sync --nohooks" "$(cat "$r/trees/tree-1/args.seen")"
check "passes no revision" "no" "$(grep -q -- --revision "$r/trees/tree-1/args.seen" && echo yes || echo no)"


# A slot whose contents cannot be read is not an empty slot: `ls` failing
# substitutes to nothing, and an `if` condition suspends `set -e`, so silence
# would be read as room to sync into.
echo "a slot that cannot be listed is not an empty slot"
r="$(fixture unreadable $ROOM 'mkdir -p src; echo RAN-THE-SYNC')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1"
printf '#!/bin/sh\necho "ls: cannot open directory: Permission denied" >&2\nexit 2\n' >"$r/bin/ls"
chmod +x "$r/bin/ls"
out="$(run "$r")"
rc=$?
nonzero "fails"        "$rc"
check   "syncs nothing" "no" "$(printf '%s' "$out" | grep -q RAN-THE-SYNC && echo yes || echo no)"
check   "publishes nothing" "no" "$(exists "$r/trees/tree-1/src")"
check   "stages nothing"    "no" "$(exists "$r/bootstrap")"

# Nothing uses a symlinked slot -- not this, and not engine-tree-pool.sh, whose
# `slots()` is a `find -type d` -- so the pool is quietly smaller than
# treeCount claims. Reporting "the pool is full" over one would be false about
# the very slot that is the problem.
echo "a slot that is a symlink is reported, not counted as full"
r="$(fixture symlinkslot $((200 * 1024 * 1024 * 1024)) 'mkdir -p src; echo RAN-THE-SYNC')"
mkdir -p "$r/trees/tree-0/src" "$r/elsewhere"
ln -s "$r/elsewhere" "$r/trees/tree-1"
out="$(run "$r")"
rc=$?
is    "fails"                     "1" "$rc"
check "counts the symlinks"       "1 slot(s) under $r/trees are symlinks" "$out"
check "says to remove the link"   "REMOVE THE LINK FIRST" "$out"
check "does not claim it is full" "no" "$(printf '%s' "$out" | grep -qF "every slot under $r/trees holds a checkout" && echo yes || echo no)"
check "syncs nothing"             "no" "$(printf '%s' "$out" | grep -q RAN-THE-SYNC && echo yes || echo no)"

# A symlinked slot alongside a real empty one is still filled: the warning is
# said, and the work that can be done is done.
echo "a symlinked slot does not stop a real one being filled"
r="$(fixture symlinkplus $((200 * 1024 * 1024 * 1024)) 'mkdir -p src; echo synced')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-2" "$r/elsewhere"
ln -s "$r/elsewhere" "$r/trees/tree-1"
out="$(run "$r")"
rc=$?
is    "succeeds"            "0" "$rc"
check "still warns"         "are symlinks" "$out"
check "fills the real slot" "filled $r/trees/tree-2" "$out"


# EVERY FIRING STARTS FROM NOTHING. What a died sync leaves is not simply
# progress -- gclient's clone resumes nothing before it moves .git into place,
# and its temp directories survive a signal -- so deciding how much of it
# counts is what the room check would then have to be told, and two attempts at
# that arithmetic failed in opposite directions.
echo "what a died sync left behind is thrown away, not resumed"
r="$(fixture stale $ROOM 'mkdir -p src; echo synced')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1" \
  "$r/bootstrap/tree-1/src/third_party/_gclient_angle_dead" \
  "$r/bootstrap/tree-1/_bad_scm/gclient_root/srcXXXX"
printf 'stale\n' >"$r/bootstrap/tree-1/.gclient"
out="$(run "$r")"
rc=$?
is    "succeeds"                   "0" "$rc"
check "the dead temp clone is gone" "no" "$(exists "$r/trees/tree-1/src/third_party/_gclient_angle_dead")"
check "_bad_scm is gone"            "no" "$(exists "$r/trees/tree-1/_bad_scm")"
check "the stale .gclient is gone"  "no" "$(printf '%s' "$(cat "$r/trees/tree-1/.gclient")" | grep -q stale && echo yes || echo no)"
check "and it publishes"            "filled $r/trees/tree-1" "$out"


# THE SEQUENCE, WHICH IS WHAT NO SINGLE FIRING SHOWS. Staging is in the same
# dataset as the trees -- that is what makes the publish a rename -- so what a
# died sync left is counted as USED by `zfs available`. Read the space before
# wiping it and the second firing refuses for exactly the bytes it is about to
# delete, every night, for ever.
echo "a died sync does not wedge the firing after it"
r="$(fixture refire 0 'mkdir -p src/third_party; echo "the network went away" >&2; exit 1')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1"
# The zfs stub MODELS THE DATASET rather than answering a constant: staging is
# inside it, so while a staged tree is there the free space is short by one.
# That is the whole mechanism, and a stub that ignored it would pass either
# way.
cat >"$r/bin/zfs" <<ZFS
#!/bin/sh
if [ -e "$r/bootstrap/tree-1/src" ]; then
  echo $((20 * 1024 * 1024 * 1024))
else
  echo $((120 * 1024 * 1024 * 1024))
fi
ZFS
chmod +x "$r/bin/zfs"
first="$(run "$r")"
check "the first firing died"       "the network went away" "$first"
check "and left what it had"        "yes" "$(exists "$r/bootstrap/tree-1/src")"

# Second firing. Read before the wipe it sees 20G and refuses for ever; wiped
# first it sees 120G, which is the truth.
printf '#!/bin/sh\nmkdir -p src; echo synced\n' >"$r/bin/gclient"
chmod +x "$r/bin/gclient"
cp "$r/bin/gclient" "$r/depot_tools/gclient"
second="$(run "$r")"
rc=$?
is    "the second firing succeeds" "0" "$rc"
check "it is not refused for room" "no" "$(printf '%s' "$second" | grep -q 'Not starting a sync' && echo yes || echo no)"
check "and it fills the slot"      "filled $r/trees/tree-1" "$second"
check "what the first left is gone" "no" "$(exists "$r/trees/tree-1/src/third_party")"


# Round seven: the wipe is the whole staging directory, not this slot's entry.
# A firing takes one slot, so a tree staged for another is one nothing comes
# back for -- and the mv -T refusal leaves exactly that.
echo "a tree staged for another slot is collected too"
r="$(fixture orphan $ROOM 'mkdir -p src; echo synced')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1" "$r/bootstrap/tree-9/src/big"
out="$(run "$r")"
rc=$?
is    "succeeds"                "0" "$rc"
check "the orphan is gone"      "no" "$(exists "$r/bootstrap/tree-9")"
check "and the slot is filled"  "filled $r/trees/tree-1" "$out"

# Staged through a symlink the bytes are on another dataset: the room check
# asks about the wrong disk, and mv -T becomes a cross-device recursive copy,
# which puts a src/ in the slot while it is still being written.
echo "a staging directory that is a symlink is refused"
r="$(fixture stagelink $ROOM 'mkdir -p src; echo RAN-THE-SYNC')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1" "$r/elsewhere"
ln -s "$r/elsewhere" "$r/bootstrap"
out="$(run "$r")"
rc=$?
check "says it is a symlink" "$r/bootstrap is a symlink" "$out"
is    "fails"                "1" "$rc"
check "syncs nothing"        "no" "$(printf '%s' "$out" | grep -q RAN-THE-SYNC && echo yes || echo no)"

# A slot holding no checkout and not empty is the one state no later firing
# mends on its own, so it must never be published into the pool.
echo "a sync that leaves no checkout publishes nothing"
r="$(fixture nosrc $ROOM 'echo synced-but-made-nothing')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1"
out="$(run "$r")"
rc=$?
check "says what is missing" "left no checkout at" "$out"
is    "fails"                "1" "$rc"
check "the slot is untouched" "no" "$(exists "$r/trees/tree-1/.gclient")"

# One wedged slot must not hold the whole pool short.
echo "a slot that needs mending does not block a healthy one"
r="$(fixture wedged $ROOM 'mkdir -p src; echo synced')"
mkdir -p "$r/trees/tree-0/src" "$r/trees/tree-1" "$r/trees/tree-2"
printf 'abc\n' >"$r/trees/tree-1/.domicile-synced-pin"
out="$(run "$r")"
rc=$?
is    "succeeds"              "0" "$rc"
check "reports the wedged one" "holds no checkout, but it is not empty" "$out"
check "fills the healthy one"  "filled $r/trees/tree-2" "$out"
check "leaves the wedged one"  "no" "$(exists "$r/trees/tree-1/src")"

echo
echo "== $pass passed, $fail failed =="
[ "$fail" -eq 0 ]
