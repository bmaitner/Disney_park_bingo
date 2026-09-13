# parkbingo

Randomized people-watching bingo cards for a day at Walt Disney World parks.

Every card is drawn from a curated table of squares
([`inst/extdata/bingo_squares.csv`](inst/extdata/bingo_squares.csv)). Each square
is tagged by mode (cynic or fan), age suitability, the parks where it applies, and
an estimate of how often players actually cross it off. Player feedback refines
that estimate over time.

*Not affiliated with or endorsed by The Walt Disney Company.*

## Installation

```r
# install.packages("remotes")
remotes::install_local(".")   # from a clone of this repo
```

## Quick start

```r
library(parkbingo)

# One cynic card for the grown-ups, drawn from squares that can happen anywhere
card <- make_bingo_card(mode = "cynic", age = "adult", seed = 2026)
card               # prints the grid in the console
bingo_odds(card)   # simulated chance of a bingo, using p_crossed

# Four kid-friendly fan cards for EPCOT, printed one per page
cards <- make_bingo_cards(4, mode = "fan", age = "child", park = "EPCOT")
save_bingo_cards(cards, "epcot_bingo.pdf")

# Use a script font for the title (any font installed on your system)
save_bingo_cards(cards, "epcot_bingo.pdf", title_family = "Segoe Script")
```

### Card options

| Argument | Values |
|---|---|
| `mode` | `"cynic"`, `"fan"`, or `"mixed"`. Squares tagged `any` fit both modes. |
| `age` | `"adult"` (all squares) or `"child"` (child-suitable squares only). |
| `park` | `"any"` or a park: `"Magic Kingdom"`/`"MK"`, `"EPCOT"`, `"Hollywood Studios"`/`"HS"`, `"Animal Kingdom"`/`"AK"`, `"Disney Springs"`, `"Typhoon Lagoon"`, `"Blizzard Beach"`. A specific park adds its own squares to the ones that can happen anywhere; `park_weight` controls how often they get picked. |
| `difficulty` | `"any"`, `"easy"` (`p_crossed >= 0.6`), `"medium"` (0.3 to 0.6), `"hard"` (`< 0.3`), or a numeric `c(min, max)` range. |
| `balance` | `TRUE` (default) draws evenly from the easy, medium, and hard thirds of the eligible squares, so cards made together are about equally hard. |
| `size`, `free_space` | Grid size (default 5) and whether the center is a free space. |

## The squares table

The table is a plain CSV so it's easy to edit by hand and review in diffs. You can
also point `bingo_squares()` at your own `.csv`, `.csv.gz`, or `.parquet` file, and
`write_squares()` writes any of those formats. Parquet needs the `nanoparquet`
package.

| Column | Description |
|---|---|
| `id` | Stable unique id (`sq0001`, ...). Feedback is linked by id, so never reuse or renumber ids. Retire a square by deleting its row. |
| `text` | Label printed on the card. Keep it short, about 40 characters or fewer. |
| `mode` | `cynic`, `fan`, or `any`. |
| `age` | `child` (fine for everyone) or `adult` (adults only). |
| `park` | `any`, or one or more park codes separated by `;`, e.g. `magic_kingdom;epcot`. See `square_vocab()`. |
| `category` | Loose grouping: `people`, `outfits`, `food`, `rides`, `merch`, `characters`, `shows`, `weather`, `park_details`. |
| `p_crossed` | Estimated share of players who cross this square off during a park day (0 to 1). Higher means easier. |
| `n_feedback` | How many player observations have informed `p_crossed`. Starts at 0 for a guess. |

Run `validate_squares(bingo_squares("path/to/file.csv"))` after editing to check
for typos in the vocabularies, duplicate ids, or out-of-range values. The test
suite also makes sure every mode, age, and park combination still has enough
squares to fill a 5x5 card.

## Refining difficulty from feedback

Every printed card shows its id at the bottom. Keep the card objects, or save
them with `saveRDS()`. After the park day, record what each player crossed off
and fold the results back into the table:

```r
squares <- bingo_squares()
fb <- rbind(
  record_feedback(cards[[1]], crossed = c("sq0017", "sq0073", "Churro")),
  record_feedback(cards[[2]], crossed = c("sq0008", "sq0125"))
)
squares <- update_difficulty(squares, fb, prior_weight = 10)
write_squares(squares, "inst/extdata/bingo_squares.csv")
```

`update_difficulty()` treats the current estimate as worth
`prior_weight + n_feedback` observations. Early guesses move quickly once real
data comes in, and well-tested estimates stay stable.
