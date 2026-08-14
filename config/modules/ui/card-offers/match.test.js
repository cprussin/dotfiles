// Unit tests for the pure button-label matcher (extension/match.js). Run with
// `node match.test.js`; wired into `nix flake check`. The DOM/engine and live
// Amex/Chase behavior are verified by hand in the browser (see README).

const assert = require("node:assert");
// match.js sits in ./extension when run from the repo, or alongside this file in
// the nix flake check (which copies both into one dir).
let matchMod;
try {
  matchMod = require("./match.js");
} catch (_e) {
  matchMod = require("./extension/match.js");
}
const { isAddLabel } = matchMod;

// Accessible-name variants Amex/Chase have shipped for the ADD control. If a real
// run stops matching, add the newly-observed label here and extend ADD_LABEL_RE.
const SHOULD_MATCH = [
  "Add to Card",
  "Add Offer",
  "Activate Offer",
  "add to list card",
  "ADD TO CARD",
];

// Labels that must NOT be clicked (already added, or the wrong control).
const SHOULD_NOT_MATCH = [
  "Added to Card",
  "Added",
  "Activated",
  "Remove offer",
  "Enrolled",
  "See details",
  "Learn more",
  "Card benefits",
  "",
  "   ",
];

let failures = 0;
for (const label of SHOULD_MATCH) {
  try {
    assert.ok(isAddLabel(label), `expected match: ${JSON.stringify(label)}`);
  } catch (e) {
    failures++;
    console.error("FAIL:", e.message);
  }
}
for (const label of SHOULD_NOT_MATCH) {
  try {
    assert.ok(!isAddLabel(label), `expected NO match: ${JSON.stringify(label)}`);
  } catch (e) {
    failures++;
    console.error("FAIL:", e.message);
  }
}

if (failures > 0) {
  console.error(`\n${failures} test(s) failed`);
  process.exit(1);
}
console.log(`ok - ${SHOULD_MATCH.length + SHOULD_NOT_MATCH.length} label assertions passed`);
