// Floating "Add all offers" button + a small toast, and an optional auto-run that
// fires once the offers list has rendered. Auto-run is on by default and can be
// toggled per-site by right-clicking the button (persisted in localStorage).
//
// Chase note: offers are per-card. This adds all offers on the CURRENTLY shown
// card; switch cards in the page and click the button again (auto-run also
// re-fires when a new batch of offers renders).

(() => {
  const BTN_ID = "card-offers-fab";
  const AUTO_KEY = "cardOffersAutoRun"; // per-origin localStorage flag
  if (document.getElementById(BTN_ID)) return; // guard against double-injection

  const autoRunEnabled = () => localStorage.getItem(AUTO_KEY) !== "off";

  function toast(message) {
    const el = document.createElement("div");
    el.textContent = message;
    Object.assign(el.style, {
      position: "fixed",
      zIndex: "2147483647",
      bottom: "72px",
      right: "20px",
      padding: "10px 14px",
      background: "rgba(0,0,0,.85)",
      color: "#fff",
      borderRadius: "8px",
      fontSize: "13px",
      fontFamily: "system-ui, sans-serif",
      maxWidth: "320px",
      boxShadow: "0 2px 8px rgba(0,0,0,.4)",
    });
    document.body.appendChild(el);
    setTimeout(() => el.remove(), 4000);
  }

  const button = document.createElement("button");
  button.id = BTN_ID;
  button.type = "button";
  button.textContent = "Add all offers";
  Object.assign(button.style, {
    position: "fixed",
    zIndex: "2147483647",
    bottom: "20px",
    right: "20px",
    padding: "10px 14px",
    background: "#1a73e8",
    color: "#fff",
    border: "none",
    borderRadius: "8px",
    cursor: "pointer",
    fontSize: "14px",
    fontFamily: "system-ui, sans-serif",
    boxShadow: "0 2px 8px rgba(0,0,0,.3)",
  });

  let running = false;
  async function run(auto) {
    if (running) return;
    running = true;
    const original = button.textContent;
    button.disabled = true;
    button.textContent = "Adding…";
    try {
      const n = await CardOffers.addAll({
        onProgress: (c) => {
          button.textContent = `Adding… (${c})`;
        },
      });
      if (n > 0) toast(`Added ${n} offer(s).`);
      else if (!auto) toast("No new offers found on this view.");
    } finally {
      button.textContent = original;
      button.disabled = false;
      running = false;
    }
  }

  button.addEventListener("click", () => run(false));

  // Right-click the button to toggle auto-run for this site.
  button.addEventListener("contextmenu", (event) => {
    event.preventDefault();
    const nowOn = autoRunEnabled();
    localStorage.setItem(AUTO_KEY, nowOn ? "off" : "on");
    toast(`Auto-run ${nowOn ? "disabled" : "enabled"} for this site.`);
  });

  document.body.appendChild(button);

  // Auto-run: wait (bounded) for the offers to render, then add them once.
  if (autoRunEnabled()) {
    let waited = 0;
    const POLL_MS = 500;
    const MAX_WAIT_MS = 20000;
    const poll = setInterval(() => {
      waited += POLL_MS;
      if (CardOffers.pendingCount() > 0) {
        clearInterval(poll);
        run(true);
      } else if (waited >= MAX_WAIT_MS) {
        clearInterval(poll);
      }
    }, POLL_MS);
  }
})();
