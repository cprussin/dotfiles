# Working in this repository

## Pull requests get reviewed by a sub-agent

Every pull request you open or modify gets a review sub-agent run against its
current head. There is no exception for one-line, follow-up or speculative
changes, and a force-push invalidates any review that came before it.

Spawn that reviewer before ending the turn in which you pushed, or say plainly
why the push needs none. `.claude/hooks/pr-review-reminder.sh` will remind you
after a push, but the rule holds whether or not the reminder fires.

The hook needs `jq`. The dev shell provides it and direnv loads that shell in a
terminal inside the checkout, so a session started there is fine; one started
from a desktop launcher or a remote environment may not have it. When the hook
can't run it warns the user in their terminal, not you -- so treat the rule as
holding whether or not you ever see a reminder.
