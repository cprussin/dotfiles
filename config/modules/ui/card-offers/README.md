# card-offers

Keep Amex/Chase card-linked offers ("Amex Offers" / "Chase Offers") added to your
cards with as little manual work as possible.

The clicking is done by a small **browser extension** (`./extension`) that runs in
your **normal, logged-in Brave** — so your password manager and device-trust are
intact and login is painless. On the offers pages it adds every available offer,
either automatically or via a floating button.

Why an extension (not the old Playwright tool): authentication can't be automated
(2FA, device-trust, bot-detection), so a human is always in the loop for login.
The trick is to make login *cheap* by staying in your real browser, and make
everything else *free*. The earlier Playwright approach did the opposite — it ran
a throwaway browser profile with no password manager, so login was the painful
part. This keeps everything in the browser you already use.

## What's here

- **`extension/`** — the browser extension (Manifest V3):
  - `manifest.json` — matches the Amex offers page and Chase (secure.chase.com).
  - `match.js` — pure logic: which button labels mean "add this offer" (tested).
  - `add-offers.js` — the engine: scan → click each add button → re-scan until done.
  - `ui.js` — a floating "Add all offers" button + optional auto-run.
- **`default.nix`** — installs the extension to a stable path, adds a `card-offers`
  launcher entry that opens both offers pages, and a weekly reminder.
- **`match.test.js`** — node unit tests for the matcher; wired into `nix flake check`.

## One-time setup (install the extension in Brave)

The extension is delivered to a stable path in your home dir; load it once:

1. Rebuild so the files are in place at
   `~/.local/share/brave-extensions/card-offers`.
2. Open `brave://extensions`, turn on **Developer mode** (top-right).
3. Click **Load unpacked** and select
   `~/.local/share/brave-extensions/card-offers`.

That's it. The path is stable across rebuilds (so the extension keeps the same
id); after a rebuild that changes the extension, restart Brave or click the
extension's **reload** to pick up the new version.

> Why not a fully automatic (policy) install? A nag-free managed-policy
> force-install needs a signed `.crx`, which means committing an RSA signing key
> to this **public** repo — bad hygiene and it trips secret scanners. If you'd
> rather go nag-free, we can add a force-install using a key kept in your
> password store / `pass` instead of the repo; ask and I'll wire it up.

## Everyday use

1. Run the **`card-offers`** launcher entry — it opens both the Amex offers page
   and the Chase dashboard in Brave (you're already logged in via your password
   manager; clear 2FA on the rare occasion it's asked).
2. On each page the extension **auto-adds** all offers once they render. If
   auto-run is off, click the floating **"Add all offers"** button. A toast
   reports how many were added.

A **weekly reminder** (Mon 10:00, via `notify-send`) nudges you so offers get
added as they refresh. Change the schedule in `default.nix`
(`systemd.user.timers.card-offers-reminder`).

### Auto-run toggle

Auto-run is **on by default**. **Right-click** the floating button to toggle it
for the current site (persisted in that site's `localStorage`).

### Chase (per-card)

Chase offers are **per-card** — there's no single "all offers" page like Amex.
The extension adds all offers on the **currently shown card**; switch cards in the
page and it re-fires (or click the button again) for each card.

### Your spouse's accounts

Your normal Brave profile can only be logged into one person's Amex/Chase at a
time. For the second person, use a separate **Brave profile** (with its own
password-manager logins); the extension runs there too. The `card-offers`
launcher opens the default profile — switch profiles in Brave for the other set.

## Verifying / fixing selectors

The add button is matched by its accessible name via `ADD_LABEL_RE` in
`extension/match.js`. This survives CSS churn but **not label rewording**. If a
run stops adding offers:

1. Open devtools on the offers page, find the real button, copy its visible text
   or `aria-label`.
2. Extend `ADD_LABEL_RE` in `extension/match.js` to cover it, and add the label
   to `SHOULD_MATCH` in `match.test.js`.
3. `node match.test.js` (or `nix flake check`) to confirm, then reload the
   extension in Brave.

Because it runs in your real browser, you can iterate live with devtools — no
special test harness needed.

## Tests

```nu
node config/modules/ui/card-offers/match.test.js   # or: nix flake check
```

Only the pure matcher is unit-tested; the DOM engine and live issuer behavior are
verified by using the extension in Brave (devtools open if something looks off).

## Security / privacy

- **No credentials anywhere.** You log in yourself in your real browser; the
  extension only clicks "add offer" buttons. It requests **no** permissions
  beyond running its content script on the two offers domains.
- The extension is scoped to `global.americanexpress.com/offers/*` and
  `secure.chase.com/*` only.
- Automated access runs against the issuers' terms; this stays tolerable by being
  your own accounts, low volume, and human-initiated (gentle click pacing, no
  headless/unattended runs).
