# Sync a Chromium checkout into an empty tree slot.
#
# Run by bootstrap-chromium-tree.service (chromium-build.nix), which is where
# the why is written down. Two seams, both so the test has neither a /build nor
# a zpool and does not want one:
#
#   BUILD_ROOT   the directory holding trees/, bootstrap/ and depot_tools/
#   POOL         the zpool whose <pool>/chromium dataset is asked for room
#
# No shebang: chromium-build.nix reads this file into `writeShellScript`, which
# supplies its own. Run it directly with `bash`.
set -eu

ROOT="${BUILD_ROOT:-/build}"
POOL="${POOL:-tank-fast}"

trees="$ROOT/trees"
staging="$ROOT/bootstrap"
depot="$ROOT/depot_tools"

# A tree is 97G, measured. chromium-build.nix's header carries the run. It is a
# BUILT tree -- a checkout plus its out/Domicile -- rather than what this sync
# alone writes, because a slot is no use to an engine job that cannot then
# build in it, and that is the budget the dataset's quota was sized to.
TREE_BYTES=$((97 * 1024 * 1024 * 1024))

if [ ! -d "$trees" ]; then
  echo "$trees does not exist, so setup-chromium-trees.service has not run." >&2
  exit 1
fi

# The same two questions cprussin/domicile's engine-depot-tools.sh asks, and
# for its reason: a depot_tools whose bootstrap has never run dies on a missing
# python3_bin_reldir.txt partway into the first command rather than at the
# start of it.
if [ ! -x "$depot/gclient" ] || [ ! -f "$depot/python3_bin_reldir.txt" ]; then
  echo "no bootstrapped depot_tools at $depot." >&2
  echo "It is a person's checkout rather than a package; run its" >&2
  echo "ensure_bootstrap, or gclient once, and this will pick it up." >&2
  exit 1
fi

# ONE SLOT PER FIRING, and the first empty one. Two syncs at once is 194G
# against a quota with room for one, and there is no hurry: the timer comes
# back tomorrow.
target=""
seen=0
linked=0
dirty=0
for slot in "$trees"/*; do
  # `-d` follows a symlink, so a slot that is one would pass the emptiness
  # pre-flight by pointing somewhere empty, take the whole sync, and then fail
  # at the rename. A slot is a directory the unit made, not a link to one.
  if [ -L "$slot" ]; then
    linked=$((linked + 1))
    continue
  fi
  if [ ! -d "$slot" ]; then continue; fi
  seen=$((seen + 1))
  if [ -d "$slot/src" ]; then continue; fi

  # A SLOT WITH NO CHECKOUT NEED NOT BE EMPTY, and the difference is hours.
  # Publishing is a rename onto the slot, which needs the slot empty -- so a
  # slot holding anything else would take the whole sync and then die on
  # `rmdir`. It is reachable by the obvious repair: everything the engine
  # writes BESIDE the checkout lands in the slot directory, because
  # $(dirname "$CHROMIUM") resolves through the symlink, so `rm -rf <slot>/src`
  # to reclaim disk leaves exactly this.
  #
  # Passed over rather than refused on, because one wedged slot blocking a
  # healthy one is a pool that stays short until somebody notices.
  #
  # Listed into a BARE assignment rather than tested as a substitution: an `ls`
  # that failed substitutes to nothing and a substitution inside an `if` is the
  # one place `set -e` is suspended, so the silence of an unreadable slot would
  # read as an empty one.
  contents=$(ls -A "$slot")
  if [ -n "$contents" ]; then
    dirty=$((dirty + 1))
    {
      echo "$slot holds no checkout, but it is not empty:"
      printf '%s\n' "$contents" | sed 's/^/  /'
      echo "Nothing can be published onto it until it is empty."
    } >&2
    continue
  fi

  target="$slot"
  break
done

# SAID EVERY FIRING, BECAUSE NOTHING ELSE WILL SAY IT. A symlinked slot is
# used by nothing -- not by this, and not by engine-tree-pool.sh, whose
# `slots()` is a `find -type d` -- so the pool is quietly smaller than
# `treeCount` claims and every symptom of that turns up somewhere else.
if [ "$linked" -gt 0 ]; then
  echo "$linked slot(s) under $trees are symlinks, which nothing uses:" >&2
  echo "the pool is that much smaller than treeCount says.  Each wants to be" >&2
  echo "a real directory -- and REMOVE THE LINK FIRST, because" >&2
  echo "setup-chromium-trees.service's mkdir -p succeeds straight through an" >&2
  echo "existing one and leaves it a link, so redeploying alone will not mend" >&2
  echo "it." >&2
fi

# Told apart because the fixes are different, and because "every slot holds a
# checkout" about a directory holding no slots is the sentence a person would
# read while the pool silently does nothing.
if [ "$seen" -eq 0 ]; then
  if [ "$linked" -gt 0 ]; then
    echo "$trees holds no slots this unit can use." >&2
  else
    echo "$trees holds no slots, so setup-chromium-trees.service did not finish." >&2
  fi
  exit 1
fi

if [ -z "$target" ]; then
  # A symlinked slot makes this NOT "nothing to do": every real slot is full,
  # but the pool is short of what it should be and only a person can mend it.
  # Saying "every slot holds a checkout" over one would be false about the very
  # slot that is the problem.
  if [ "$linked" -gt 0 ] || [ "$dirty" -gt 0 ]; then
    echo "no slot under $trees can be filled: every one either holds a" >&2
    echo "checkout already or wants the mending named above." >&2
    exit 1
  fi
  echo "every slot under $trees holds a checkout"
  exit 0
fi

# STAGED OUTSIDE THE POOL, AND THIS IS THE LOAD-BEARING PART.
# engine-tree-pool.sh calls a slot usable the moment it holds a src/, and
# PREFERS one carrying no pin -- so a sync writing straight into the slot would
# be handed to a build within minutes of creating src/, hours before it is a
# tree. Under $staging the pool cannot see it, and the rename at the end is
# what publishes it.
#
# EVERY FIRING STARTS FROM NOTHING. Keeping what a died sync left looks free
# and is not: gclient clones the solution into a sibling temp directory and
# moves its `.git` into `src/` as the last act, so a clone killed before that
# resumes nothing -- and the `finally:` that removes those temp directories
# runs on a Python exception and NOT on a signal, so a kill strands them under
# fresh random names that nothing collects. Deciding how much of it counts is
# what the room check would then have to be told, and two attempts at exactly
# that arithmetic failed in opposite directions.
#
# AND IT IS CLEARED BEFORE THE SPACE IS READ, WHICH IS THE ORDER RATHER THAN
# THE ARITHMETIC. What a died sync left is in this dataset -- that is what
# makes the publish an intra-dataset rename -- so `zfs available` counts it as
# used. Read first and wiped second, a died sync that got further than the free
# margin makes every firing afterwards refuse for exactly the bytes it is about
# to delete, for ever. Wiped first, `available` is the truth.
#
# AND IT IS THE WHOLE DIRECTORY, not this slot's entry in it. A firing takes
# one slot, so a tree staged for a DIFFERENT slot is one nothing comes back
# for -- and the `mv -T` refusal at the end leaves exactly that, by name. Left
# alone it is 97G no firing deletes and every firing is charged for: the same
# wedge, one slot over. There is never a second staged tree to protect, because
# the unit is a oneshot that takes one slot at a time.
#
# A SYMLINK HERE WOULD UNDO BOTH, and the tmpfiles rule that declares this
# directory does not stop one: systemd-tmpfiles' `d` leaves an existing symlink
# alone. Staged through a link the bytes are on another dataset, so the check
# below asks about the wrong disk -- and `mv -T` becomes a cross-device
# recursive COPY, which puts a `src/` into the slot while it is still being
# written. That is the half-synced tree handed to a build that the staging
# exists to prevent.
if [ -L "$staging" ]; then
  echo "$staging is a symlink, and staging must be a real directory in $ROOT:" >&2
  echo "the free-space check asks about this dataset, and the publish is a" >&2
  echo "rename within it.  Remove the link; the tmpfiles rule remakes the" >&2
  echo "directory on the next deploy." >&2
  exit 1
fi

work="$staging/$(basename "$target")"
rm -rf "$staging"

# Refused here rather than found out as EDQUOT four hours in, which is the
# bargain setup-chromium-trees.service's quota check already makes.
avail=$(zfs get -Hp -o value available "$POOL/chromium")

# `[ x -lt y ]` on a non-number exits 2, and an `if` condition suspends `set -e`
# -- so without this the guard reads "not less than" and starts the sync, which
# is the one outcome it exists to prevent. A `zfs` that exits non-zero is
# already caught: the bare assignment above aborts under `set -e`.
case "$avail" in
  '' | *[!0-9]*)
    echo "zfs answered \"$avail\" for the space free on $POOL/chromium." >&2
    echo "That is not a number of bytes, so it is not room." >&2
    exit 1
    ;;
esac

if [ "$avail" -lt "$TREE_BYTES" ]; then
  echo "$POOL/chromium has $((avail / 1024 / 1024 / 1024))G free and a tree is 97G." >&2
  echo "Not starting a sync there is no room for." >&2
  exit 1
fi

# Made after the room check rather than before it, so a refusal leaves nothing
# behind to be counted against the next one.
mkdir -p "$work"

# Written here rather than left to `fetch`, which on an older depot_tools marks
# the solution `managed` -- that syncs src to the head of its branch, so every
# engine run would lay its series over a revision nobody pinned. engine-sync.sh
# refuses such a tree by name, hours later.
#
# Unindented because a .gclient is exec'd as Python: a leading space on the
# first statement is an IndentationError.
cat >"$work/.gclient" <<'GCLIENT'
solutions = [
  {
    "name": "src",
    "url": "https://chromium.googlesource.com/chromium/src.git",
    "managed": False,
    "custom_deps": {},
    "custom_vars": {},
  },
]
GCLIENT

# --nohooks keeps every prebuilt binary but depot_tools' own python out of
# this: the toolchain downloads are hooks, and the first engine job runs them
# inside upstream's FHS shell, which is where they work. It is also why this
# cannot be engine-sync.sh -- that shell is $target/src/tools/nix/shell.nix and
# there is no src yet.
#
# No revision, and no .domicile-synced-pin written afterwards. A slot with a
# checkout and no stamp reads to the pool as carrying no pin, which is true and
# is what makes it the slot the next repin takes instead of evicting tree-0.
echo "syncing a checkout into $work for $target"
cd "$work"
PATH="$depot:$PATH" gclient sync --nohooks

# WHAT THE SYNC WAS FOR, asked rather than assumed. A `gclient sync` that exits
# 0 without leaving a `src/` would otherwise publish a slot holding nothing but
# the `.gclient` written above -- and such a slot holds no checkout and is not
# empty, which is the one state no later firing mends on its own.
if [ ! -d "$work/src" ]; then
  echo "the sync exited 0 but left no checkout at $work/src." >&2
  echo "Publishing that would put a slot into the pool that holds no tree and" >&2
  echo "cannot be filled again until somebody empties it." >&2
  exit 1
fi

# An intra-dataset rename, so it is instant and nothing is ever half visible.
#
# The slot was empty when this started, so reaching a failure here means
# something else wrote into it during the sync. Said out loud, because the bare
# coreutils message is the cryptic failure the pre-flight above exists to stop
# anyone getting.
rmdir "$target" || {
  echo "$target could not be removed -- coreutils names the reason above -- so" >&2
  echo "the tree staged in $work cannot be published into it.  Clear whatever" >&2
  echo "is holding the slot; the next firing syncs again from nothing." >&2
  exit 1
}
mv -T "$work" "$target" || {
  echo "the rename of $work onto $target failed -- coreutils names the reason" >&2
  echo "above.  The slot has already been removed to make room for it, so the" >&2
  echo "tree is in $work and the slot is gone: setup-chromium-trees.service" >&2
  echo "remakes the slot on the next deploy, and the firing after that syncs" >&2
  echo "into it again." >&2
  exit 1
}
echo "filled $target"
