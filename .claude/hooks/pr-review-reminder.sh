#!/usr/bin/env bash
#
# Reminds Claude Code to spawn a reviewer after it pushes.
#
# The rule itself is in CLAUDE.md, but a document is read once at session start
# and then competes with everything else in context.  A hook on the push itself
# puts the instruction in front of the model at the one moment it applies.
#
# This reminds; it does not compel.  PostToolUse output is advisory context,
# and the model can read it and still end the turn.  Only a Stop hook can
# refuse to end one, and blocking there loops when a reviewer legitimately
# isn't needed.

set -eu

# Anything that goes wrong in here leaves the hook doing nothing, and a hook
# doing nothing looks exactly like a session that never pushed.  Say so out
# loud instead.  It has to be a systemMessage on stdout rather than a line on
# stderr: Claude Code only surfaces a hook's stderr when the hook exits
# non-zero, and exiting non-zero here would report a broken push instead.
warn() {
  printf '{"systemMessage":"pr-review-reminder: %s"}\n' "$1"
  exit 0
}

# `--version` as well as `command -v`, because jq present but not working --
# a dangling symlink, a wrong-arch binary, a wrapper that errors -- fails the
# same silent way jq missing does.
if ! command -v jq > /dev/null 2>&1 || ! jq --version > /dev/null 2>&1
then
  warn "jq is missing or not working, so PR-review reminders are off"
fi

payload=$(cat)
event=$(printf '%s' "$payload" | jq -r '.hook_event_name // "PostToolUse"') \
  || warn "could not read hook_event_name from the hook payload"
command=$(printf '%s' "$payload" | jq -r '.tool_input.command // ""') \
  || warn "could not read tool_input.command from the hook payload"

# Split the command on the shell operators that separate one command from the
# next, drop any segment whose push is a dry run, and look for a git push in
# what's left.  Splitting first is what makes `git push --dry-run && git push`
# fire while `git push --dry-run` alone doesn't.
#
# The dry-run test only counts -n and --dry-run appearing *after* `push`, so
# `xargs -n 1 git push` and `sort -n f && git push` still fire -- an unrelated
# -n earlier in the segment isn't git's.
#
# The leading boundary is "not part of a longer word" rather than "at a command
# position", so `sudo git push`, `time git push`, `nix develop -c git push` and
# a leading VAR=value all still match, while `legit pushed` doesn't.  That
# direction is deliberate: a missed push means a PR goes unreviewed, which
# defeats the hook, whereas a spurious reminder costs one wasted reviewer
# spawn.  The loose end goes where it's cheap.
#
# No `set -o pipefail` here, deliberately: `grep -Eq` exits at the first match
# and SIGPIPEs the grep feeding it, so under pipefail the pipeline would go
# non-zero on exactly the inputs that should fire, and the hook would go quiet.
# shellcheck disable=SC2020  # a set of chars is exactly what's wanted here
if ! printf '%s' "$command" \
  | tr ';&|' '\n\n\n' \
  | grep -Ev -- 'push[[:space:]]+([^[:space:]]+[[:space:]]+)*(-n|--dry-run)([[:space:]]|$)' \
  | grep -Eq '(^|[^[:alnum:]_./-])git[[:space:]]+.*push([^[:alnum:]_-]|$)'
then
  exit 0
fi

# PostToolUseFailure covers a push chained ahead of something that failed --
# the Bash tool throws on any non-zero exit -- but it also fires for a push the
# remote rejected, or one that was interrupted or timed out.  Claiming a push
# completed when it didn't would send a reviewer after a head that was never
# pushed, so that path says what actually happened and lets the model check.
case $event in
  PostToolUseFailure)
    lead="A command containing a git push just ran and failed. If the push \
itself landed, the rule below applies; confirm before acting on it."
    ;;
  *)
    lead="A git push just completed."
    ;;
esac

jq -nc --arg event "$event" --arg lead "$lead" '{
  hookSpecificOutput: {
    hookEventName: $event,
    additionalContext: (
      $lead
      + " This repository asks that every pull request you open or modify"
      + " gets a review sub-agent run against its current head, with no"
      + " exception for one-line, follow-up or speculative changes, and a"
      + " force-push invalidates any review that came before it. Spawn that"
      + " reviewer before ending this turn, or say plainly why the push"
      + " needs none."
    )
  }
}'
