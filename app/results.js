// Finished-card results, sent anonymously to the inbox so squares' p_crossed
// estimates can be refined with update_difficulty() in R.

import { completedLines } from "./bingo.js";
import { newId } from "./outbox.js";

export function summarizeCard(card) {
  const squares = card.grid.filter((id) => id !== null).length;
  const spotted = card.crossed.filter((c, i) => c && card.grid[i] !== null).length;
  const bingos = completedLines(card.crossed, card.settings.size).length;
  return { squares, spotted, bingos };
}

// One entry per square (the free space is left out). Contains no personal
// information: just the card's settings, times, and what was crossed off.
export function buildResult(card, endedAt = new Date()) {
  const { mode, age, park, difficulty, size } = card.settings;
  return {
    type: "result",
    submission_id: newId(),
    card_id: card.cardId,
    mode,
    age,
    park,
    difficulty,
    size,
    started_at: card.created,
    ended_at: endedAt.toISOString(),
    squares: card.grid
      .map((id, i) => (id === null ? null : { id, crossed: Boolean(card.crossed[i]) }))
      .filter(Boolean),
  };
}
