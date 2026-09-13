// Card logic for the phone app. Mirrors filter_squares(), make_bingo_card()
// and card_audience() in the R package; app/tests.html checks they agree.

// Relative share of squares drawn from the hardest, middle, and easiest thirds.
export const DIFFICULTY_MIX = {
  any: [1, 1, 1],
  easy: [1, 2, 3],
  medium: [1, 4, 1],
  hard: [3, 2, 1],
};

export function filterSquares(squares, { mode, age, park }) {
  return squares.filter(
    (s) =>
      (mode === "mixed" || s.mode === mode || s.mode === "any") &&
      (age === "adult" || s.age === "child") &&
      s.park.some((p) => p === "any" || p === park)
  );
}

// Small seedable PRNG so tests are reproducible. Returns floats in [0, 1).
export function makeRng(seed = Date.now()) {
  let a = seed >>> 0;
  return function () {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function shuffle(items, rng) {
  const out = items.slice();
  for (let i = out.length - 1; i > 0; i--) {
    const j = Math.floor(rng() * (i + 1));
    [out[i], out[j]] = [out[j], out[i]];
  }
  return out;
}

// Draw n items without replacement, with probability proportional to weight.
function sampleWeighted(items, n, weightOf, rng) {
  const pool = items.slice();
  const picked = [];
  while (picked.length < n && pool.length > 0) {
    const total = pool.reduce((sum, item) => sum + weightOf(item), 0);
    let r = rng() * total;
    let i = 0;
    while (i < pool.length - 1 && r >= weightOf(pool[i])) {
      r -= weightOf(pool[i]);
      i++;
    }
    picked.push(pool.splice(i, 1)[0]);
  }
  return picked;
}

// Draw n squares from thirds of the pool ordered by p_crossed (hardest first),
// in proportion to mix.
function sampleBalanced(pool, n, weightOf, rng, mix = DIFFICULTY_MIX.any) {
  const ordered = shuffle(pool, rng).sort((a, b) => a.p_crossed - b.p_crossed);
  const k = Math.min(mix.length, ordered.length);
  if (k < mix.length) mix = Array(k).fill(1);
  const bins = Array.from({ length: k }, (_, b) =>
    ordered.slice(Math.round((b * ordered.length) / k),
                  Math.round(((b + 1) * ordered.length) / k))
  );
  const total = mix.reduce((a, b) => a + b, 0);
  const quotas = mix.map((m) => Math.floor((n * m) / total));
  const extra = n - quotas.reduce((a, b) => a + b, 0);
  shuffle([...bins.keys()], rng)
    .slice(0, extra)
    .forEach((b) => quotas[b]++);

  let picked = bins.flatMap((bin, b) =>
    sampleWeighted(bin, Math.min(quotas[b], bin.length), weightOf, rng)
  );
  const shortfall = n - picked.length;
  if (shortfall > 0) {
    const ids = new Set(picked.map((s) => s.id));
    const rest = pool.filter((s) => !ids.has(s.id));
    picked = picked.concat(sampleWeighted(rest, shortfall, weightOf, rng));
  }
  return picked;
}

export function makeCard(squares, settings, rng = makeRng()) {
  const {
    mode, age, park, difficulty = "any",
    size = 5, freeSpace = true, balance = true, parkWeight = 2,
  } = settings;
  const hasFree = freeSpace && size % 2 === 1;
  const pool = filterSquares(squares, { mode, age, park });
  const needed = size * size - (hasFree ? 1 : 0);
  if (pool.length < needed) {
    throw new Error(
      `Only ${pool.length} squares match these settings; a ${size}x${size} card needs ${needed}.`
    );
  }

  const weightOf = (s) =>
    park !== "any" && !s.park.includes("any") ? parkWeight : 1;
  const drawn = balance || difficulty !== "any"
    ? sampleBalanced(pool, needed, weightOf, rng, DIFFICULTY_MIX[difficulty])
    : sampleWeighted(pool, needed, weightOf, rng);

  const center = (size * size - 1) / 2;
  let cells = [...Array(size * size).keys()];
  if (hasFree) cells = cells.filter((c) => c !== center);
  cells = shuffle(cells, rng);

  // grid[i] is a square id, or null for the free space; row-major order.
  // Labels are stored on the card so saved cards survive squares updates.
  const grid = Array(size * size).fill(null);
  drawn.forEach((s, i) => { grid[cells[i]] = s.id; });

  const hex = "0123456789abcdef";
  const cardId = Array.from({ length: 6 }, () => hex[Math.floor(rng() * 16)]).join("");

  const textOf = new Map(drawn.map((s) => [s.id, s.text]));
  return {
    cardId,
    settings: { mode, age, park, difficulty, size, freeSpace: hasFree },
    grid,
    labels: grid.map((id) => (id === null ? null : textOf.get(id))),
    crossed: grid.map((id) => id === null),
    created: new Date().toISOString(),
  };
}

// All rows, columns and both diagonals, as lists of cell indices.
export function winningLines(size) {
  const lines = [];
  const idx = (r, c) => r * size + c;
  for (let r = 0; r < size; r++) lines.push([...Array(size).keys()].map((c) => idx(r, c)));
  for (let c = 0; c < size; c++) lines.push([...Array(size).keys()].map((r) => idx(r, c)));
  lines.push([...Array(size).keys()].map((i) => idx(i, i)));
  lines.push([...Array(size).keys()].map((i) => idx(i, size - 1 - i)));
  return lines;
}

export function completedLines(crossed, size) {
  return winningLines(size).filter((line) => line.every((i) => crossed[i]));
}

export function cardAudience(settings, parks) {
  const who = { cynic: "cynics", fan: "fans", mixed: "fans and cynics" }[settings.mode];
  const where = settings.park === "any"
    ? "any park"
    : parks.find((p) => p.code === settings.park).label;
  return `${settings.age === "child" ? "Child" : "Adult"} ${who} at ${where}`;
}
