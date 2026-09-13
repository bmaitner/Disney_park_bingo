import { makeCard, filterSquares, completedLines, cardAudience } from "./bingo.js";

const STORAGE_KEY = "parkbingo.v1";
const MAX_HISTORY = 100;

const $ = (id) => document.getElementById(id);
const views = { setup: $("setup-view"), card: $("card-view") };

let data = null; // contents of squares.json
let state = { settings: null, card: null, history: [] };

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
  $("subtitle").textContent =
    name === "card" && state.card
      ? cardAudience(state.card.settings, data.parks)
      : "Set up your card";
  if (name === "card") fitAllLabels();
}

function readSettings() {
  const form = new FormData($("setup-form"));
  return {
    age: form.get("age"),
    mode: form.get("mode"),
    park: form.get("park"),
    difficulty: "any", // No picker yet: most settings lack enough easy or hard squares.
  };
}

function applySettings(settings) {
  if (!settings) return;
  const form = $("setup-form");
  for (const key of ["age", "mode"]) {
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

function archiveCard(card) {
  if (!card || crossedCount(card) === 0) return;
  state.history.push({ ...card, ended: new Date().toISOString() });
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

// Shrink each label until it fits its cell.
function fitAllLabels() {
  for (const cell of $("board").children) {
    const label = cell.firstElementChild;
    cell.classList.remove("tight");
    let size = cell.classList.contains("free") ? 20 : 15;
    label.style.fontSize = `${size}px`;
    const fits = () =>
      label.scrollHeight <= cell.clientHeight - 8 &&
      label.scrollWidth <= label.clientWidth + 1;
    while (!fits() && size > 9) {
      size -= 0.5;
      label.style.fontSize = `${size}px`;
    }
    if (!fits()) cell.classList.add("tight");
  }
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

  $("park").replaceChildren(
    ...data.parks.map((p) => new Option(p.code === "any" ? "Any park" : p.label, p.code))
  );

  $("setup-form").addEventListener("submit", dealCard);
  $("setup-form").addEventListener("change", updatePoolHint);
  $("back-to-card").addEventListener("click", () => show("card"));
  $("new-card").addEventListener("click", openSetup);
  $("board").addEventListener("click", toggleCell);

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
