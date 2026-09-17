import { makeCard, filterSquares, completedLines, cardAudience } from "./bingo.js";
import { buildSuggestion, validateSuggestion, MAX_TEXT } from "./suggest.js";
import { flushOutbox, sendCount } from "./outbox.js";
import { buildResult, summarizeCard } from "./results.js";
import { SUBMIT_URL } from "./config.js";
import { fitLabels } from "./fit.js";
import { printQuery } from "./printing.js";

const STORAGE_KEY = "parkbingo.v1";
const MAX_HISTORY = 100;

const $ = (id) => document.getElementById(id);
const views = {
  setup: $("setup-view"),
  card: $("card-view"),
  done: $("done-view"),
  suggest: $("suggest-view"),
};

let data = null; // contents of squares.json
let state = { settings: null, card: null, history: [], outbox: [], shareResults: true };
let returnTo = "setup"; // view to go back to after suggesting

// Storage ------------------------------------------------------------------

function loadState() {
  try {
    const saved = JSON.parse(localStorage.getItem(STORAGE_KEY));
    if (saved) state = { ...state, ...saved };
  } catch {
    // Private mode or blocked storage: play without saving.
  }
}

function saveState() {
  try {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
  } catch {
    // Ignore; progress just won't survive a reload.
  }
}

// Views --------------------------------------------------------------------

function show(name) {
  for (const [key, el] of Object.entries(views)) el.hidden = key !== name;
  const subtitles = { setup: "Set up your card", done: "Done playing?", suggest: "Suggest a square" };
  $("subtitle").textContent =
    name === "card" && state.card
      ? cardAudience(state.card.settings, data.parks)
      : subtitles[name];
  window.scrollTo(0, 0);
  if (name === "card") fitAllLabels();
}

function readSettings() {
  const form = new FormData($("setup-form"));
  return {
    age: form.get("age"),
    mode: form.get("mode"),
    park: form.get("park"),
    difficulty: form.get("difficulty"),
  };
}

function applySettings(settings) {
  if (!settings) return;
  const form = $("setup-form");
  for (const key of ["age", "mode", "difficulty"]) {
    const input = form.querySelector(`input[name="${key}"][value="${settings[key]}"]`);
    if (input) input.checked = true;
  }
  if ([...form.park.options].some((o) => o.value === settings.park)) {
    form.park.value = settings.park;
  }
}

// Show how many squares match, and block dealing when there aren't enough.
function updatePoolHint() {
  const n = filterSquares(data.squares, readSettings()).length;
  const enough = n >= 24;
  $("deal").disabled = !enough;
  $("setup-error").hidden = true;
  $("pool-hint").textContent = enough
    ? `${n} squares to draw from`
    : `Only ${n} squares fit these settings (24 needed). Try another park or mode.`;
  $("pool-hint").className = enough ? "hint" : "error";
}

function openSetup() {
  applySettings(state.settings);
  updatePoolHint();
  $("back-to-card").hidden = !state.card;
  show("setup");
}

// Card ---------------------------------------------------------------------

function crossedCount(card) {
  return card.crossed.filter((c, i) => c && card.grid[i] !== null).length;
}

// Keep a local record of every card. Only cards finished with "Done playing"
// and sharing turned on are sent anywhere.
function archiveCard(card, { finished = false, shared = false } = {}) {
  if (!card) return;
  state.history.push({ ...card, ended: new Date().toISOString(), finished, shared });
  state.history = state.history.slice(-MAX_HISTORY);
}

function dealCard(event) {
  event.preventDefault();
  const settings = readSettings();
  let card;
  try {
    card = makeCard(data.squares, settings);
  } catch (err) {
    $("setup-error").textContent = err.message;
    $("setup-error").hidden = false;
    return;
  }
  archiveCard(state.card);
  state.settings = settings;
  state.card = card;
  saveState();
  renderCard();
  show("card");
  sendCount(SUBMIT_URL, "card");
}

function renderCard() {
  const card = state.card;
  const size = card.settings.size;
  const board = $("board");
  board.style.setProperty("--size", size);
  board.replaceChildren();

  card.grid.forEach((id, i) => {
    const cell = document.createElement("button");
    cell.type = "button";
    cell.className = "cell";
    cell.dataset.index = i;
    if ((i + 1) % size === 0) cell.classList.add("last-col");
    if (i >= size * (size - 1)) cell.classList.add("last-row");

    const label = document.createElement("span");
    label.className = "label";
    if (id === null) {
      cell.classList.add("free");
      cell.disabled = true;
      label.textContent = "FREE";
    } else {
      label.textContent = card.labels[i];
    }
    cell.append(label);
    board.append(cell);
  });

  $("card-id").textContent = `card ${card.cardId}`;
  updateMarks(false);
}

function updateMarks(celebrate) {
  const card = state.card;
  const size = card.settings.size;
  const lines = completedLines(card.crossed, size);
  const winning = new Set(lines.flat());

  for (const cell of $("board").children) {
    const i = Number(cell.dataset.index);
    cell.setAttribute("aria-pressed", String(card.crossed[i]));
    cell.classList.toggle("win", winning.has(i));
  }

  const total = card.grid.filter((id) => id !== null).length;
  $("progress").textContent = `${crossedCount(card)} of ${total} spotted`;

  const banner = $("banner");
  banner.classList.toggle("off", lines.length === 0);
  banner.textContent = lines.length > 1 ? `Bingo x${lines.length}!` : "Bingo!";
  if (celebrate) {
    banner.classList.remove("pop");
    void banner.offsetWidth; // restart the animation
    banner.classList.add("pop");
    navigator.vibrate?.([60, 40, 120]);
  }
}

function toggleCell(event) {
  const cell = event.target.closest(".cell");
  if (!cell || cell.disabled) return;
  const card = state.card;
  const i = Number(cell.dataset.index);
  const before = completedLines(card.crossed, card.settings.size).length;
  card.crossed[i] = !card.crossed[i];
  const after = completedLines(card.crossed, card.settings.size).length;
  saveState();
  updateMarks(after > before);
}

// Done playing ---------------------------------------------------------------

function openDone() {
  const { squares, spotted, bingos } = summarizeCard(state.card);
  $("done-spotted").textContent = `You spotted ${spotted} of ${squares}`;
  $("done-bingos").textContent =
    bingos === 0 ? "" : bingos === 1 ? "and got a bingo!" : `and got ${bingos} bingos!`;
  $("share-row").hidden = !SUBMIT_URL;
  $("share-results").checked = state.shareResults;
  show("done");
}

async function finishCard() {
  const card = state.card;
  const share = Boolean(SUBMIT_URL) && $("share-results").checked;
  if (SUBMIT_URL) state.shareResults = $("share-results").checked;
  if (share) state.outbox.push({ payload: buildResult(card), attempts: 0 });
  archiveCard(card, { finished: true, shared: share });
  state.card = null;
  saveState();
  openSetup();
  if (!share) return;
  const pending = state.outbox.length;
  await sendOutbox();
  toast(state.outbox.length < pending
    ? "Thanks for sharing your results!"
    : "Thanks! Results will send when you have signal.");
}

// Suggestions ----------------------------------------------------------------

function openSuggest() {
  returnTo = views.card.hidden ? "setup" : "card";
  const form = $("suggest-form");
  form.reset();
  const current = state.card?.settings ?? state.settings ?? readSettings();
  const mode = current.mode === "mixed" ? "any" : current.mode;
  form.querySelector(`input[name="mode"][value="${mode}"]`).checked = true;
  form.querySelector(`input[name="age"][value="${current.age}"]`).checked = true;
  form.park.value = current.park;
  updateSuggestCount();
  $("suggest-error").hidden = true;
  show("suggest");
}

function updateSuggestCount() {
  $("suggest-count").textContent = `${$("suggest-text").value.length} / ${MAX_TEXT}`;
}

function toast(message) {
  const el = $("toast");
  el.textContent = message;
  el.hidden = false;
  clearTimeout(toast.timer);
  toast.timer = setTimeout(() => { el.hidden = true; }, 3500);
}

async function sendOutbox() {
  if (!SUBMIT_URL || state.outbox.length === 0 || sendOutbox.busy) return 0;
  sendOutbox.busy = true;
  const before = state.outbox.slice();
  try {
    const remaining = await flushOutbox(before, SUBMIT_URL);
    // Keep anything queued while we were sending.
    state.outbox = remaining.concat(state.outbox.slice(before.length));
    saveState();
    return before.length - remaining.length;
  } finally {
    sendOutbox.busy = false;
  }
}

async function submitSuggestion(event) {
  event.preventDefault();
  const form = new FormData($("suggest-form"));
  if (form.get("website")) {
    show(returnTo); // bot filled the trap; quietly do nothing
    return;
  }
  const suggestion = buildSuggestion({
    text: form.get("text"),
    mode: form.get("mode"),
    age: form.get("age"),
    park: form.get("park"),
    note: form.get("note"),
  });
  const error = validateSuggestion(suggestion, data.parks.map((p) => p.code));
  if (error) {
    $("suggest-error").textContent = error;
    $("suggest-error").hidden = false;
    return;
  }

  state.outbox.push({ payload: suggestion, attempts: 0 });
  saveState();
  show(returnTo);
  const pending = state.outbox.length;
  await sendOutbox();
  toast(state.outbox.length < pending
    ? "Thanks! Your suggestion was sent."
    : "Thanks! It'll send when you have signal.");
}

function fitAllLabels() {
  fitLabels($("board").children);
}

// Start --------------------------------------------------------------------

async function start() {
  loadState();
  try {
    const response = await fetch("squares.json");
    data = await response.json();
  } catch {
    $("loading").textContent = "Couldn't load the squares. Check your connection and reload.";
    return;
  }
  $("loading").hidden = true;

  const parkOptions = () =>
    data.parks.map((p) => new Option(p.code === "any" ? "Any park" : p.label, p.code));
  $("park").replaceChildren(...parkOptions());
  $("suggest-park").replaceChildren(...parkOptions());

  $("setup-form").addEventListener("submit", dealCard);
  $("setup-form").addEventListener("change", updatePoolHint);
  $("print-cards").addEventListener("click", () => {
    location.href = `print.html${printQuery(readSettings())}`;
  });
  $("back-to-card").addEventListener("click", () => show("card"));
  $("new-card").addEventListener("click", openSetup);
  $("done-playing").addEventListener("click", openDone);
  $("finish-card").addEventListener("click", finishCard);
  $("keep-playing").addEventListener("click", () => show("card"));
  $("board").addEventListener("click", toggleCell);

  if (SUBMIT_URL) {
    document.querySelectorAll(".suggest-open").forEach((button) => {
      button.hidden = false;
      button.addEventListener("click", openSuggest);
    });
    $("suggest-form").addEventListener("submit", submitSuggestion);
    $("suggest-text").addEventListener("input", updateSuggestCount);
    $("suggest-cancel").addEventListener("click", () => show(returnTo));
  }
  if (SUBMIT_URL) {
    window.addEventListener("online", sendOutbox);
    document.addEventListener("visibilitychange", () => {
      if (document.visibilityState === "visible") sendOutbox();
    });
    sendOutbox();
    sendCount(SUBMIT_URL, "open");
  }

  let resizeTimer;
  window.addEventListener("resize", () => {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(() => { if (!views.card.hidden) fitAllLabels(); }, 100);
  });

  if (state.card) {
    renderCard();
    show("card");
  } else {
    openSetup();
  }

  if ("serviceWorker" in navigator && location.protocol !== "file:") {
    navigator.serviceWorker.register("sw.js").catch(() => {});
  }
}

start();
