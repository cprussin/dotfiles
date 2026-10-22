// Gmail Theme Sync
// Switches Gmail's own theme (Dark / Default) to match prefers-color-scheme,
// by driving Gmail's theme picker the way you would by hand.
//
// Gmail's markup is obfuscated, so this finds controls by their accessible
// labels and visible text (English UI). If Google changes those, adjust LABELS.

(() => {
  "use strict";

  const LABELS = {
    settingsButton: ["Settings"],
    viewAll: ["View all"],
    themes: { dark: ["Dark"], light: ["Default"] },
    save: ["Save"],
  };

  const DEBUG = false;
  const log = (...a) => DEBUG && console.log("[gmail-theme-sync]", ...a);

  const media = window.matchMedia("(prefers-color-scheme: dark)");
  const wanted = () => (media.matches ? "dark" : "light");

  // ---- Is Gmail currently dark? Sample the background behind the top bar.
  function luminance(rgb) {
    const m = rgb.match(/rgba?\(([\d.]+),\s*([\d.]+),\s*([\d.]+)(?:,\s*([\d.]+))?/);
    if (!m || (m[4] !== undefined && Number(m[4]) === 0)) return null;
    const [r, g, b] = [m[1], m[2], m[3]].map((v) => {
      v = Number(v) / 255;
      return v <= 0.03928 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4;
    });
    return 0.2126 * r + 0.7152 * g + 0.0722 * b;
  }

  function currentTheme() {
    const points = [
      [window.innerWidth / 2, 12],
      [window.innerWidth - 40, window.innerHeight / 2],
    ];
    for (const [x, y] of points) {
      let el = document.elementFromPoint(x, y);
      while (el && el !== document.documentElement) {
        const l = luminance(getComputedStyle(el).backgroundColor);
        if (l !== null) return l < 0.2 ? "dark" : "light";
        el = el.parentElement;
      }
    }
    return null;
  }

  // ---- DOM helpers
  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

  function visible(el) {
    const r = el.getBoundingClientRect();
    return r.width > 0 && r.height > 0;
  }

  function findByLabel(labels, root = document) {
    for (const label of labels) {
      for (const el of root.querySelectorAll(
        `[aria-label="${label}"], [data-tooltip="${label}"], [title="${label}"]`
      )) {
        if (visible(el)) return el;
      }
    }
    return null;
  }

  function findByText(labels, root = document) {
    const els = root.querySelectorAll('button, [role="button"], a, span, div');
    for (const label of labels) {
      for (const el of els) {
        if (el.children.length <= 2 && el.textContent.trim() === label && visible(el)) {
          return el.closest('button, [role="button"], a') || el;
        }
      }
    }
    return null;
  }

  async function waitFor(fn, timeout = 4000) {
    const start = Date.now();
    while (Date.now() - start < timeout) {
      const el = fn();
      if (el) return el;
      await sleep(100);
    }
    return null;
  }

  function click(el) {
    for (const type of ["mousedown", "mouseup", "click"]) {
      el.dispatchEvent(new MouseEvent(type, { bubbles: true, cancelable: true, view: window }));
    }
  }

  // ---- The switch
  let busy = false;

  async function sync() {
    if (busy || document.hidden) return;
    const want = wanted();
    const have = currentTheme();
    log("want", want, "have", have);
    if (!have || have === want) return;

    // Don't fight Gmail: one attempt per system-scheme value per tab.
    const key = "gmail-theme-sync:attempted";
    if (sessionStorage.getItem(key) === want) return;
    sessionStorage.setItem(key, want);

    busy = true;
    try {
      const gear = await waitFor(() => findByLabel(LABELS.settingsButton));
      if (!gear) return log("no settings button");
      click(gear);

      // The quick-settings panel sometimes shows the tile directly.
      let tile = await waitFor(() => findByLabel(LABELS.themes[want]), 1500);
      if (!tile) {
        const viewAll = await waitFor(() => findByText(LABELS.viewAll));
        if (!viewAll) return log("no 'View all'");
        click(viewAll);
        tile = await waitFor(() => findByLabel(LABELS.themes[want]));
      }
      if (!tile) return log("no theme tile for", want);
      click(tile);

      const save = await waitFor(() => findByText(LABELS.save), 1500);
      if (save) click(save);
      else document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape", bubbles: true }));

      await sleep(800);
      if (currentTheme() === want) sessionStorage.removeItem(key);
      log("done; now", currentTheme());
    } finally {
      busy = false;
    }
  }

  media.addEventListener("change", () => {
    sessionStorage.removeItem("gmail-theme-sync:attempted");
    sync();
  });
  document.addEventListener("visibilitychange", sync);

  // Gmail renders late; give it a moment after load.
  waitFor(() => findByLabel(LABELS.settingsButton), 20000).then(() => setTimeout(sync, 1000));
})();
