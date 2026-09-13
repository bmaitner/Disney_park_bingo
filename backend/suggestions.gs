/**
 * Theme park bingo: suggestion inbox.
 *
 * A Google Apps Script web app, bound to a Google Sheet. The phone app POSTs
 * square suggestions here; each becomes a row in the "suggestions" tab for you
 * to review. The R function fetch_suggestions() reads them back using a
 * private read token. See backend/README.md for setup.
 */

const SUGGESTIONS_SHEET = "suggestions";
const COLUMNS = [
  "received_at", "submission_id", "text", "mode", "age", "park", "note", "status",
];
const MAX_TEXT = 60;
const MAX_NOTE = 200;
const MAX_PER_HOUR = 300; // across all players; guards against floods

const MODES = ["cynic", "fan", "any"];
const AGES = ["child", "adult"];
const PARKS = [
  "any", "magic_kingdom", "epcot", "hollywood_studios", "animal_kingdom",
  "disney_springs", "typhoon_lagoon", "blizzard_beach",
];

function doPost(e) {
  try {
    const body = JSON.parse(e.postData.contents);
    if (body.type === "suggestion") return json(addSuggestion(body));
    if (body.type === "list_suggestions") return json(listSuggestions(body));
    return json({ ok: false, error: "unknown request type" });
  } catch (err) {
    return json({ ok: false, error: "bad request" });
  }
}

// Health check: open the web app URL in a browser to confirm it's deployed.
function doGet() {
  return json({ ok: true, service: "parkbingo" });
}

function addSuggestion(body) {
  // Honeypot: the app never fills this hidden field; bots often do.
  if (body.website) return { ok: true };

  const s = {
    submission_id: clean(body.submission_id, 64),
    text: clean(body.text, MAX_TEXT),
    mode: body.mode,
    age: body.age,
    park: body.park,
    note: clean(body.note, MAX_NOTE),
  };
  if (!/^[A-Za-z0-9-]{8,64}$/.test(s.submission_id)) return rejected("invalid id");
  if (s.text.length < 3) return rejected("text too short");
  if (MODES.indexOf(s.mode) < 0) return rejected("invalid mode");
  if (AGES.indexOf(s.age) < 0) return rejected("invalid age");
  if (PARKS.indexOf(s.park) < 0) return rejected("invalid park");

  const lock = LockService.getScriptLock();
  lock.waitLock(10000);
  try {
    const sheet = getSheet();
    // The app retries when offline, so the same submission can arrive twice.
    const seen = sheet.getRange("B:B").createTextFinder(s.submission_id)
      .matchEntireCell(true).findNext();
    if (seen) return { ok: true, duplicate: true };

    const cache = CacheService.getScriptCache();
    const hourKey = "count-" + Math.floor(Date.now() / 3600000);
    const count = Number(cache.get(hourKey) || 0);
    if (count >= MAX_PER_HOUR) return { ok: false, error: "busy", retry: true };
    cache.put(hourKey, String(count + 1), 3600);

    sheet.appendRow([
      new Date().toISOString(), s.submission_id, asText(s.text), s.mode, s.age,
      s.park, asText(s.note), "",
    ]);
    return { ok: true };
  } finally {
    lock.releaseLock();
  }
}

function listSuggestions(body) {
  const token = PropertiesService.getScriptProperties().getProperty("READ_TOKEN");
  if (!token || body.token !== token) return { ok: false, error: "unauthorized" };

  const values = getSheet().getDataRange().getDisplayValues();
  const rows = values.slice(1).map(function (row) {
    const out = {};
    COLUMNS.forEach(function (name, i) { out[name] = row[i] || ""; });
    return out;
  });
  return { ok: true, suggestions: rows };
}

/**
 * Run once from the Apps Script editor (select it and click Run). Creates the
 * private token that fetch_suggestions() uses and prints it to the log.
 */
function createReadToken() {
  const token = (Utilities.getUuid() + Utilities.getUuid()).replace(/-/g, "");
  PropertiesService.getScriptProperties().setProperty("READ_TOKEN", token);
  Logger.log("PARKBINGO_TOKEN=" + token);
}

function getSheet() {
  const book = SpreadsheetApp.getActiveSpreadsheet();
  let sheet = book.getSheetByName(SUGGESTIONS_SHEET);
  if (!sheet) {
    sheet = book.insertSheet(SUGGESTIONS_SHEET);
    sheet.appendRow(COLUMNS);
    sheet.setFrozenRows(1);
  }
  return sheet;
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
