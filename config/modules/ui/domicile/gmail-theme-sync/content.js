// Gmail Theme Sync
// Switches Gmail's own theme (Dark / Default) to match prefers-color-scheme,
// by driving Gmail's theme picker the way you would by hand.
//
// Gmail's markup is obfuscated, so this finds controls by their accessible
// labels and visible text (English UI). If Google changes those, adjust LABELS.
//
// To see what it does, run this in a Gmail tab's console and reload:
//   localStorage.setItem("gmail-theme-sync:debug", "1")

(() => {
  "use strict";

  const LABELS = {
    settingsButton: ["Settings"],
    viewAll: ["View all"],
    // Only in the open quick-settings panel.
    quickSettings: ["See all settings"],
    pickerTitle: ["Pick your theme", "Themes"],
    themes: { dark: ["Dark"], light: ["Default"] },
    save: ["Save"],
    cancel: ["Cancel"],
  };

  const KEY = {
    debug: "gmail-theme-sync:debug",
    // `{want, at}` of the last switch that failed. A load doesn't retry it for
    // a day, so a broken switch doesn't open the picker on every load; a
    // change of scheme always does.
    failed: "gmail-theme-sync:failed",
  };
  const RETRY_AFTER_MS = 24 * 60 * 60 * 1000;

  const store = {
    get(key) {
      try {
        return localStorage.getItem(key);
      } catch {
        return null;
      }
    },
    set(key, value) {
      try {
        localStorage.setItem(key, value);
      } catch {}
    },
    remove(key) {
      try {
        localStorage.removeItem(key);
      } catch {}
    },
  };

  const log = (...a) =>
    store.get(KEY.debug) && console.log("[gmail-theme-sync]", ...a);

  const media = window.matchMedia("(prefers-color-scheme: dark)");
  const wanted = () => (media.matches ? "dark" : "light");

  // ---- Is Gmail currently dark? Sample the background behind the top bar.
  function luminance(rgb) {
    const m = rgb.match(/rgba?\(([\d.]+),\s*([\d.]+),\s*([\d.]+)(?:,\s*([\d.]+))?/);
    // Skip see-through layers (a fading backdrop) for what is behind them.
    if (!m || (m[4] !== undefined && Number(m[4]) < 0.5)) return null;
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

  const CLICKABLE = 'button, [role="button"], [role="option"], [role="radio"], a';

  // By accessible name: aria-label, tooltip, title, or an image's alt text.
  function findByLabel(labels, root = document) {
    for (const label of labels) {
      for (const el of root.querySelectorAll(
        `[aria-label="${label}"], [data-tooltip="${label}"], [title="${label}"], img[alt="${label}"]`
      )) {
        if (visible(el)) return el.closest(CLICKABLE) || el;
      }
    }
    return null;
  }

  function findByText(labels, root = document) {
    const els = root.querySelectorAll(`${CLICKABLE}, span, div`);
    for (const label of labels) {
      for (const el of els) {
        if (el.children.length <= 2 && el.textContent.trim() === label && visible(el)) {
          return el.closest(CLICKABLE) || el;
        }
      }
    }
    return null;
  }

  const find = (labels, root) => findByLabel(labels, root) || findByText(labels, root);

  // The open dialog whose text includes one of `titles`.
  function findDialog(titles) {
    for (const el of document.querySelectorAll('[role="dialog"], [role="alertdialog"]')) {
      if (visible(el) && titles.some((t) => el.textContent.includes(t))) return el;
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
    const r = el.getBoundingClientRect();
    const at = {
      bubbles: true,
      cancelable: true,
      view: window,
      clientX: r.left + r.width / 2,
      clientY: r.top + r.height / 2,
    };
    for (const type of ["mousedown", "mouseup", "click"]) {
      el.dispatchEvent(new MouseEvent(type, at));
    }
  }

  function escape() {
    const target = document.activeElement || document.body;
    for (const type of ["keydown", "keyup"]) {
      target.dispatchEvent(
        new KeyboardEvent(type, { key: "Escape", code: "Escape", keyCode: 27, bubbles: true })
      );
    }
  }

  // ---- The switch
  let busy = false;
  // A scheme change that arrived mid-switch, run once that one is done.
  let pending = false;

  // Drive the picker to `want`. True if Gmail took it.
  async function switchTo(want) {
    let gear = null;
    let picker = null;
    let ok = false;
    try {
      gear = await waitFor(() => findByLabel(LABELS.settingsButton));
      if (!gear) return log("no settings button"), false;
      click(gear);

      // Go straight to the full picker: the quick-settings panel also has
      // "Default" density and inbox-type options, which a label search there
      // would click instead of the theme.
      const viewAll = await waitFor(() => findByText(LABELS.viewAll));
      if (!viewAll) return log("no 'View all'"), false;
      click(viewAll);

      picker = await waitFor(() => findDialog(LABELS.pickerTitle));
      if (!picker) return log("no theme picker"), false;

      const tile = await waitFor(() => find(LABELS.themes[want], picker));
      if (!tile) return log("no theme tile for", want, picker), false;
      click(tile);

      const save = await waitFor(() => findByText(LABELS.save, picker), 1500);
      if (!save) return log("no 'Save'"), false;
      click(save);
      ok = true;

      const took = !!(await waitFor(() => currentTheme() === want, 5000));
      log("done; now", currentTheme());
      return took;
    } finally {
      // Never leave Gmail's picker or quick settings open.
      if (!ok) {
        const cancel = picker && document.contains(picker) && findByText(LABELS.cancel, picker);
        if (cancel) click(cancel);
        else escape();
        await sleep(300);
      }
      if (gear && findByText(LABELS.quickSettings)) click(gear);
    }
  }

  async function sync({ force = false } = {}) {
    if (busy) {
      pending ||= force;
      return;
    }
    if (document.hidden) {
      // Hidden by the time a queued re-run comes up: catch it up on return.
      changed ||= force;
      return;
    }
    const want = wanted();
    const have = currentTheme();
    log("want", want, "have", have);
    if (!have || have === want) return;

    let failed = null;
    try {
      failed = JSON.parse(store.get(KEY.failed));
    } catch {}
    if (!force && failed?.want === want && Date.now() - failed.at < RETRY_AFTER_MS) {
      return log("switch to", want, "failed recently; not retrying until the scheme changes");
    }

    busy = true;
    try {
      if (await switchTo(want)) store.remove(KEY.failed);
      else store.set(KEY.failed, JSON.stringify({ want, at: Date.now() }));
    } finally {
      busy = false;
    }
    if (pending) {
      pending = false;
      sync({ force: true });
    }
  }

  // A change while the tab is hidden is caught up on when it comes back.
  let changed = false;
  media.addEventListener("change", () => {
    changed = true;
    if (!document.hidden) {
      changed = false;
      sync({ force: true });
    }
  });
  document.addEventListener("visibilitychange", () => {
    const force = changed;
    if (!document.hidden) changed = false;
    sync({ force });
  });

  // Gmail renders late; give it a moment after load.
  waitFor(() => findByLabel(LABELS.settingsButton), 20000).then(() => setTimeout(sync, 1000));
})();
