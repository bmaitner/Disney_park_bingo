/**
 * Theme park bingo: player inbox.
 *
 * A Google Apps Script web app, bound to a Google Sheet. The phone app POSTs
 * here:
 *   - square suggestions, one row each in the "suggestions" tab
 *   - finished-card results, one row per square in the "results" tab
 *   - app opens, cards dealt, and cards printed, counted per day in the
 *     "visits" tab
 * The R functions fetch_suggestions(), fetch_results() and fetch_visits() read
 * them back using a private read token. See backend/README.md for setup.
 */

const SHEETS = {
  suggestions: [
    "received_at", "submission_id", "text", "mode", "age", "park", "note", "status",
  ],
  results: [
    "received_at", "submission_id", "card_id", "mode", "age", "park", "difficulty",
    "started_at", "ended_at", "square_id", "crossed",
  ],
  visits: ["date", "visits", "cards", "printed"],
};
const MAX_TEXT = 60;
const MAX_NOTE = 200;
// Across all players; guards against floods.
const MAX_PER_HOUR = { suggestions: 300, results: 300, visits: 5000 };
// Which column of the visits tab each counted event adds to.
const VISIT_COLUMNS = { open: 2, card: 3, print: 4 };
const MAX_PRINTED = 12;

const SQUARE_MODES = ["cynic", "fan", "any"];
const CARD_MODES = ["cynic", "fan", "mixed"];
const AGES = ["child", "adult"];
const DIFFICULTIES = ["any", "easy", "medium", "hard"];
const PARKS = [
  "any", "magic_kingdom", "epcot", "hollywood_studios", "animal_kingdom",
  "disney_springs", "typhoon_lagoon", "blizzard_beach",
];

function doPost(e) {
  try {
    const body = JSON.parse(e.postData.contents);
    if (body.type === "suggestion") return json(addSuggestion(body));
    if (body.type === "result") return json(addResult(body));
    if (body.type === "visit") return json(addVisit(body));
    if (body.type === "list_suggestions") return json(listRows(body, "suggestions"));
    if (body.type === "list_results") return json(listRows(body, "results"));
    if (body.type === "list_visits") return json(listRows(body, "visits"));
    return json({ ok: false, error: "unknown request type" });
  } catch (err) {
    return json({ ok: false, error: "bad request" });
  }
}

// Health check: open the web app URL in a browser to confirm it's deployed.
function doGet() {
  return json({ ok: true, service: "parkbingo", accepts: ["suggestion", "result", "visit"] });
}

function addSuggestion(body) {
  // Honeypot: the app never fills this hidden field; bots often do.
  if (body.website) return { ok: true };

  const s = {
    submission_id: clean(body.submission_id, 64),
    text: clean(body.text, MAX_TEXT),
    note: clean(body.note, MAX_NOTE),
  };
  if (!validId(s.submission_id)) return rejected("invalid id");
  if (s.text.length < 3) return rejected("text too short");
  if (SQUARE_MODES.indexOf(body.mode) < 0) return rejected("invalid mode");
  if (AGES.indexOf(body.age) < 0) return rejected("invalid age");
  if (PARKS.indexOf(body.park) < 0) return rejected("invalid park");

  return appendRows("suggestions", s.submission_id, [[
    new Date().toISOString(), s.submission_id, asText(s.text), body.mode, body.age,
    body.park, asText(s.note), "",
  ]]);
}

function addResult(body) {
  const id = clean(body.submission_id, 64);
  if (!validId(id)) return rejected("invalid id");
  if (!/^[0-9a-f]{6}$/.test(body.card_id)) return rejected("invalid card id");
  if (CARD_MODES.indexOf(body.mode) < 0) return rejected("invalid mode");
  if (AGES.indexOf(body.age) < 0) return rejected("invalid age");
  if (PARKS.indexOf(body.park) < 0) return rejected("invalid park");
  if (DIFFICULTIES.indexOf(body.difficulty) < 0) return rejected("invalid difficulty");

  const started = Date.parse(body.started_at);
  const ended = Date.parse(body.ended_at);
  if (isNaN(started) || isNaN(ended) || ended < started) return rejected("invalid times");

  const squares = body.squares;
  if (!Array.isArray(squares) || squares.length < 1 || squares.length > 48) {
    return rejected("invalid squares");
  }
  const seen = {};
  for (let i = 0; i < squares.length; i++) {
    const sq = squares[i];
    if (!sq || !/^sq\d{4}$/.test(sq.id) || typeof sq.crossed !== "boolean" || seen[sq.id]) {
      return rejected("invalid squares");
    }
    seen[sq.id] = true;
  }

  const received = new Date().toISOString();
  const startedIso = new Date(started).toISOString();
  const endedIso = new Date(ended).toISOString();
  const rows = squares.map(function (sq) {
    return [
      received, id, body.card_id, body.mode, body.age, body.park, body.difficulty,
      startedIso, endedIso, sq.id, sq.crossed,
    ];
  });
  return appendRows("results", id, rows);
}

// Add to today's count (UTC) of app opens, cards dealt, or cards printed.
// Nothing about the visitor is kept. Requests without an event are app opens.
function addVisit(body) {
  const event = body.event == null ? "open" : body.event;
  const column = VISIT_COLUMNS[event];
  if (!column) return rejected("invalid event");
  const n = body.n == null ? 1 : body.n;
  if (!Number.isInteger(n) || n < 1 || n > MAX_PRINTED) return rejected("invalid count");

  const lock = LockService.getScriptLock();
  lock.waitLock(10000);
  try {
    const cache = CacheService.getScriptCache();
    const hourKey = "visits-" + Math.floor(Date.now() / 3600000);
    const count = Number(cache.get(hourKey) || 0);
    if (count >= MAX_PER_HOUR.visits) return { ok: true, skipped: true };
    cache.put(hourKey, String(count + 1), 3600);

    const sheet = getSheet("visits");
    const columns = SHEETS.visits;
    // Tabs made before cards were counted only have date and visits.
    if (sheet.getLastColumn() < columns.length) {
      sheet.getRange(1, 1, 1, columns.length).setValues([columns]);
    }
    const today = new Date().toISOString().slice(0, 10);
    const last = sheet.getLastRow();
    // Days are appended in order, so today's row is always the last one.
    if (last > 1 && sheet.getRange(last, 1).getDisplayValue() === today) {
      const cell = sheet.getRange(last, column);
      cell.setValue(String(Number(cell.getDisplayValue()) + n));
    } else {
      const row = columns.map(function (_, i) { return i === 0 ? today : "0"; });
      row[column - 1] = String(n);
      sheet.appendRow(row);
    }
    return { ok: true };
  } finally {
    lock.releaseLock();
  }
}

// Append rows for one submission, skipping repeats of the same submission_id.
function appendRows(kind, submissionId, rows) {
  const lock = LockService.getScriptLock();
  lock.waitLock(10000);
  try {
    const sheet = getSheet(kind);
    // The app retries when offline, so the same submission can arrive twice.
    const repeat = sheet.getRange("B:B").createTextFinder(submissionId)
      .matchEntireCell(true).findNext();
    if (repeat) return { ok: true, duplicate: true };

    const cache = CacheService.getScriptCache();
    const hourKey = kind + "-" + Math.floor(Date.now() / 3600000);
    const count = Number(cache.get(hourKey) || 0);
    if (count >= MAX_PER_HOUR[kind]) return { ok: false, error: "busy", retry: true };
    cache.put(hourKey, String(count + 1), 3600);

    const start = sheet.getLastRow() + 1;
    const shortBy = start + rows.length - 1 - sheet.getMaxRows();
    if (shortBy > 0) sheet.insertRowsAfter(sheet.getMaxRows(), Math.max(shortBy, 500));
    sheet.getRange(start, 1, rows.length, rows[0].length).setValues(rows);
    return { ok: true };
  } finally {
    lock.releaseLock();
  }
}

function listRows(body, kind) {
  const token = PropertiesService.getScriptProperties().getProperty("READ_TOKEN");
  if (!token || body.token !== token) return { ok: false, error: "unauthorized" };

  const columns = SHEETS[kind];
  const values = getSheet(kind).getDataRange().getDisplayValues();
  const rows = values.slice(1).map(function (row) {
    const out = {};
    columns.forEach(function (name, i) { out[name] = row[i] || ""; });
    return out;
  });
  const out = { ok: true };
  out[kind] = rows;
  return out;
}

/**
 * Run once from the Apps Script editor (select it and click Run). Creates the
 * private token that fetch_suggestions() and fetch_results() use and prints it
 * to the log. Running it again replaces the token.
 */
function createReadToken() {
  const token = (Utilities.getUuid() + Utilities.getUuid()).replace(/-/g, "");
  PropertiesService.getScriptProperties().setProperty("READ_TOKEN", token);
  Logger.log("PARKBINGO_TOKEN=" + token);
}

function getSheet(kind) {
  const book = SpreadsheetApp.getActiveSpreadsheet();
  let sheet = book.getSheetByName(kind);
  if (!sheet) {
    const columns = SHEETS[kind];
    sheet = book.insertSheet(kind);
    // Keep timestamps and ids as plain text so they read back unchanged.
    sheet.getRange(1, 1, sheet.getMaxRows(), columns.length).setNumberFormat("@");
    sheet.appendRow(columns);
    sheet.setFrozenRows(1);
  }
  return sheet;
}

function validId(id) {
  return /^[A-Za-z0-9-]{8,64}$/.test(id);
}

function clean(value, maxLength) {
  return String(value == null ? "" : value)
    .replace(/[\u0000-\u001f\u007f]/g, " ")
    .replace(/\s+/g, " ")
    .trim()
    .slice(0, maxLength);
}

// Stop Sheets from treating text like "=IMPORTXML(...)" as a formula.
function asText(value) {
  return /^[=+\-@]/.test(value) ? "'" + value : value;
}

function rejected(reason) {
  return { ok: false, error: reason };
}

function json(obj) {
  return ContentService.createTextOutput(JSON.stringify(obj))
    .setMimeType(ContentService.MimeType.JSON);
}
