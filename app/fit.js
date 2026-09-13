// Shrink card labels until they fit their cells. Works on layout sizes, so it
// also measures correctly inside CSS-scaled previews.

function fits(cell, label, pad) {
  return label.scrollHeight <= cell.clientHeight - pad &&
    label.scrollWidth <= label.clientWidth + 1;
}

function fitOne(cell, max, min, pad) {
  const label = cell.firstElementChild;
  cell.classList.remove("tight");
  let size = max;
  label.style.fontSize = `${size}px`;
  while (!fits(cell, label, pad) && size > min) {
    size -= 0.5;
    label.style.fontSize = `${size}px`;
  }
  if (!fits(cell, label, pad)) cell.classList.add("tight");
  return size;
}

// cells: elements whose first child is the label. Free-space cells (class
// "free") use freeMax. With uniform, labels share one size (the 25th
// percentile of their best fits) so a card looks even, and only labels that
// don't fit at that size shrink further.
export function fitLabels(cells, { max = 15, freeMax = 20, min = 9, pad = 8, uniform = false } = {}) {
  const list = [...cells];
  const sizes = list.map((cell) =>
    fitOne(cell, cell.classList.contains("free") ? freeMax : max, min, pad)
  );
  if (!uniform) return;

  const regular = sizes.filter((_, i) => !list[i].classList.contains("free")).sort((a, b) => a - b);
  if (regular.length === 0) return;
  const shared = regular[Math.floor((regular.length - 1) * 0.25)];
  list.forEach((cell, i) => {
    if (!cell.classList.contains("free") && sizes[i] > shared) fitOne(cell, shared, min, pad);
  });
}
