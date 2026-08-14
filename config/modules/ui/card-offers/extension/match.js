// Pure button-label matching for the offer adder. No DOM access here so it can be
// unit-tested in node (see ../match.test.js) as well as loaded as a content
// script. Amex/Chase label the "add this offer" control by its accessible name;
// matching by name survives CSS churn but not label *rewording* — if a real run
// stops finding offers, read the button's text in devtools and extend
// ADD_LABEL_RE (and add the observed label to match.test.js).

// Buttons that ADD an offer.
const ADD_LABEL_RE = /\b(add\s+(to\s+)?(card|list\s+card|offer)|activate\s+offer)\b/i;

// Labels that mean the offer is already on the card (or would REMOVE it) — never
// click these, even if ADD_LABEL_RE would otherwise match ("Added to Card").
const ADDED_LABEL_RE = /\b(added|activated|remove|enrolled)\b/i;

function isAddLabel(text) {
  const t = (text || "").trim();
  if (!t) return false;
  return ADD_LABEL_RE.test(t) && !ADDED_LABEL_RE.test(t);
}

// Export for node tests; harmless no-op as a content script (module is undefined).
if (typeof module !== "undefined" && module.exports) {
  module.exports = { ADD_LABEL_RE, ADDED_LABEL_RE, isAddLabel };
}
