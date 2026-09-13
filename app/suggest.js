// Square suggestions: validation and an offline outbox that sends to the
// Apps Script inbox in backend/suggestions.gs. Limits match that script.

export const MAX_TEXT = 60;
export const MAX_NOTE = 200;
const MAX_ATTEMPTS = 20;

function newId() {
  if (globalThis.crypto?.randomUUID) return crypto.randomUUID();
  return `${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 12)}`;
}

export function buildSuggestion({ text, mode, age, park, note = "" }) {
  return {
    type: "suggestion",
    submission_id: newId(),
    text: text.replace(/\s+/g, " ").trim(),
    mode,
    age,
    park,
    note: note.replace(/\s+/g, " ").trim(),
    created_at: new Date().toISOString(),
  };
}

// Returns an error message for the player, or null if the suggestion is fine.
export function validateSuggestion(s, parkCodes) {
  if (s.text.length < 3) return "Describe the square in a few words.";
  if (s.text.length > MAX_TEXT) return `Keep it to ${MAX_TEXT} characters so it fits on a card.`;
  if (s.note.length > MAX_NOTE) return `Keep the note to ${MAX_NOTE} characters.`;
  if (!["cynic", "fan", "any"].includes(s.mode)) return "Pick a mode.";
  if (!["child", "adult"].includes(s.age)) return "Pick who it's for.";
  if (!parkCodes.includes(s.park)) return "Pick a park.";
  return null;
}

// Try to send every queued suggestion. Returns the items still waiting.
// Items the inbox rejects as invalid are dropped; network failures and "busy"
// responses stay queued (the inbox ignores duplicates, so retries are safe).
export async function flushOutbox(outbox, url, fetchImpl = fetch) {
  const remaining = [];
  for (const item of outbox) {
    let keep = true;
    try {
      const response = await fetchImpl(url, {
        method: "POST",
        // text/plain avoids a CORS preflight, which Apps Script can't answer.
        headers: { "Content-Type": "text/plain;charset=utf-8" },
        body: JSON.stringify(item.payload),
      });
      const result = await response.json();
      keep = !result.ok && Boolean(result.retry);
    } catch {
      keep = true;
    }
    if (keep) {
      const attempts = (item.attempts ?? 0) + 1;
      if (attempts < MAX_ATTEMPTS) remaining.push({ ...item, attempts });
    }
  }
  return remaining;
}
