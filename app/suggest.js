// Square suggestions for the Apps Script inbox in backend/inbox.gs.
// Limits match that script.

import { newId } from "./outbox.js";

export const MAX_TEXT = 60;
export const MAX_NOTE = 200;

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
