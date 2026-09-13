import { cardAudience, filterSquares } from "./bingo.js";
import { fitLabels } from "./fit.js";
import { MAX_CARDS, makePrintCards, newSeed, printQuery, readPrintSettings } from "./printing.js";

const $ = (id) => document.getElementById(id);
const SHEET_WIDTH_IN = 7.4; // fits Letter and A4 inside the page margins

let data = null;
let settings = null;

function formSettings() {
  const form = new FormData($("print-form"));
  return {
    age: form.get("age"),
    mode: form.get("mode"),
    park: form.get("park"),
    difficulty: form.get("difficulty"),
    count: Number(form.get("count")),
    seed: settings.seed,
  };
}

function applyToForm(s) {
  const form = $("print-form");
  for (const key of ["age", "mode", "difficulty"]) {
    form.querySelector(`input[name="${key}"][value="${s[key]}"]`).checked = true;
  }
  form.park.value = s.park;
  form.count.value = String(s.count);
}

function buildSheet(card) {
  const size = card.settings.size;
  const sheet = document.createElement("article");
  sheet.className = "sheet";

  const title = document.createElement("h2");
  title.className = "sheet-title";
  title.textContent = "Theme park bingo";

  const subtitle = document.createElement("p");
  subtitle.className = "sheet-subtitle";
  subtitle.textContent = cardAudience(card.settings, data.parks);

  const board = document.createElement("div");
  board.className = "board";
  board.style.setProperty("--size", size);
  card.grid.forEach((id, i) => {
    const cell = document.createElement("div");
    cell.className = "cell";
    if ((i + 1) % size === 0) cell.classList.add("last-col");
    if (i >= size * (size - 1)) cell.classList.add("last-row");
    const label = document.createElement("span");
    label.className = "label";
    if (id === null) {
      cell.classList.add("free");
      label.textContent = "FREE";
    } else {
      label.textContent = card.labels[i];
    }
    cell.append(label);
    board.append(cell);
  });

  const footer = document.createElement("p");
  footer.className = "sheet-footer";
  footer.textContent = `card ${card.cardId}`;

  sheet.append(title, subtitle, board, footer);
  const frame = document.createElement("div");
  frame.className = "sheet-frame";
  frame.append(sheet);
  return frame;
}

// Scale the true-size sheets down to fit the screen; printing ignores this.
function scalePreview() {
  const available = document.documentElement.clientWidth - 24;
  const scale = Math.min(1, available / (SHEET_WIDTH_IN * 96));
  $("sheets").style.setProperty("--scale", scale);
}

function render() {
  settings = formSettings();
  history.replaceState(null, "", printQuery(settings));

  const pool = filterSquares(data.squares, settings).length;
  const error = $("print-error");
  const sheets = $("sheets");
  if (pool < 24) {
    error.textContent = `Only ${pool} squares fit these settings (24 needed). Try another park or mode.`;
    error.hidden = false;
    $("print").disabled = true;
    sheets.replaceChildren();
    return;
  }
  error.hidden = true;
  $("print").disabled = false;

  const cards = makePrintCards(data.squares, settings);
  sheets.replaceChildren(...cards.map(buildSheet));
  scalePreview();
  for (const board of sheets.querySelectorAll(".board")) {
    fitLabels(board.children, { max: 18, freeMax: 28, min: 9, pad: 12, uniform: true });
  }
}

async function start() {
  try {
    data = await (await fetch("squares.json")).json();
  } catch {
    $("loading").textContent = "Couldn't load the squares. Check your connection and reload.";
    return;
  }
  $("loading").hidden = true;

  const parkCodes = data.parks.map((p) => p.code);
  $("park").replaceChildren(
    ...data.parks.map((p) => new Option(p.code === "any" ? "Any park" : p.label, p.code))
  );
  $("count").replaceChildren(
    ...Array.from({ length: MAX_CARDS }, (_, i) => new Option(`${i + 1}`, `${i + 1}`))
  );

  settings = readPrintSettings(new URLSearchParams(location.search), parkCodes);
  applyToForm(settings);
  render();

  $("print-form").addEventListener("change", (event) => {
    // Changing only the count keeps the same first cards.
    if (event.target.name !== "count") settings.seed = newSeed();
    render();
  });
  $("reshuffle").addEventListener("click", () => {
    settings.seed = newSeed();
    render();
  });
  $("print").addEventListener("click", () => window.print());

  let resizeTimer;
  window.addEventListener("resize", () => {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(scalePreview, 100);
  });
}

start();
