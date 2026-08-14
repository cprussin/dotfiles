// The add-all engine. Scans the page for add-offer buttons (by accessible name,
// via isAddLabel from match.js), clicks each with a gentle delay, re-scans as the
// DOM mutates, and stops after a few scroll+scan rounds find nothing new. Kept
// deliberately gentle — this runs against the issuers' terms and is tolerated
// because it's your own accounts, low volume, and human-initiated.

const CardOffers = (() => {
  const SETTLE_MS = 700; // pause between clicks; rapid-fire clicking looks robotic
  const QUIET_ROUNDS_TO_STOP = 3;

  const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

  function labelOf(el) {
    return (el.getAttribute("aria-label") || el.innerText || el.textContent || "").trim();
  }

  function isClickable(el) {
    if (el.dataset.cardOffersClicked) return false;
    if (el.disabled || el.getAttribute("aria-disabled") === "true") return false;
    const rect = el.getBoundingClientRect();
    return rect.width > 0 && rect.height > 0;
  }

  function candidateButtons() {
    const els = document.querySelectorAll('button, [role="button"], a[role="button"]');
    return Array.from(els).filter((el) => isClickable(el) && isAddLabel(labelOf(el)));
  }

  function pendingCount() {
    return candidateButtons().length;
  }

  // Add every offer on the page. Returns the number of buttons clicked.
  async function addAll({ onProgress } = {}) {
    let added = 0;
    let quiet = 0;
    while (quiet < QUIET_ROUNDS_TO_STOP) {
      window.scrollBy(0, 20000); // reveal lazy-loaded offers
      await sleep(SETTLE_MS);

      const buttons = candidateButtons();
      if (buttons.length === 0) {
        quiet += 1;
        continue;
      }
      quiet = 0;

      for (const btn of buttons) {
        try {
          btn.dataset.cardOffersClicked = "1"; // mark first so a re-scan skips it
          btn.scrollIntoView({ block: "center" });
          btn.click();
          added += 1;
          if (onProgress) onProgress(added);
          await sleep(SETTLE_MS);
        } catch (_err) {
          // ignore a single button that won't take a click; keep going
        }
      }
    }
    return added;
  }

  return { addAll, pendingCount, candidateButtons };
})();
