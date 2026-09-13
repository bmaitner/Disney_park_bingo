// Settings for printable cards, stored in the page URL so a link reproduces
// the same set of cards.

import { makeCard, makeRng } from "./bingo.js";

export const MAX_CARDS = 12;

const CHOICES = {
  age: ["adult", "child"],
  mode: ["fan", "cynic", "mixed"],
  difficulty: ["any", "easy", "hard"],
};

const DEFAULTS = { age: "child", mode: "fan", park: "any", difficulty: "any", count: 4 };

export function newSeed() {
  return Math.floor(Math.random() * 1e9);
}

export function readPrintSettings(params, parkCodes) {
  const pick = (key) => (CHOICES[key].includes(params.get(key)) ? params.get(key) : DEFAULTS[key]);
  const count = Number.parseInt(params.get("n"), 10);
  const seed = Number.parseInt(params.get("seed"), 10);
  return {
    age: pick("age"),
    mode: pick("mode"),
    difficulty: pick("difficulty"),
    park: parkCodes.includes(params.get("park")) ? params.get("park") : DEFAULTS.park,
    count: Number.isFinite(count) ? Math.min(Math.max(count, 1), MAX_CARDS) : DEFAULTS.count,
    seed: Number.isFinite(seed) && seed >= 0 ? seed : newSeed(),
  };
}

export function printQuery(settings) {
  const params = new URLSearchParams({
    age: settings.age,
    mode: settings.mode,
    park: settings.park,
    difficulty: settings.difficulty,
  });
  if (settings.count != null) params.set("n", String(settings.count));
  if (settings.seed != null) params.set("seed", String(settings.seed));
  return `?${params}`;
}

// The same settings and seed always give the same cards.
export function makePrintCards(squares, settings) {
  const rng = makeRng(settings.seed);
  return Array.from({ length: settings.count }, () => makeCard(squares, settings, rng));
}
