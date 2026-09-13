# Sync the phone app in app/ with the R package.
#
# Run from the repo root after editing inst/extdata/bingo_squares.csv:
#   Rscript tools/update_app.R          # squares + test expectations
#   Rscript tools/update_app.R --icons  # also redraw the app icons

devtools::load_all(quiet = TRUE)

# Squares -----------------------------------------------------------------
export_squares_json("app/squares.json")

# Expectations for app/tests.html -------------------------------------------
# The app reimplements filtering in JavaScript; these R results let the
# in-browser tests check that both implementations agree.
squares <- bingo_squares()
combos <- expand.grid(
  mode = c("cynic", "fan", "mixed"),
  age = c("adult", "child"),
  park = square_vocab()$parks,
  stringsAsFactors = FALSE
)
combos$n <- mapply(
  function(mode, age, park) nrow(filter_squares(squares, mode, age, park)),
  combos$mode, combos$age, combos$park
)
combos$audience <- mapply(
  function(mode, age, park) {
    card_audience(structure(list(mode = mode, age = age, park = park),
                            class = "bingo_card"))
  },
  combos$mode, combos$age, combos$park
)
writeLines(
  jsonlite::toJSON(combos, pretty = TRUE),
  "app/tests/expected.json"
)

# Icons -------------------------------------------------------------------
draw_icon <- function(file, px, pad) {
  grDevices::png(file, width = px, height = px, type = "cairo", bg = "#FFFBF3")
  on.exit(grDevices::dev.off())
  grid::grid.newpage()
  n <- 3
  side <- 1 - 2 * pad
  cell <- side / n
  marked <- c(1, 5, 9)
  for (i in seq_len(n^2)) {
    r <- (i - 1) %/% n
    cc <- (i - 1) %% n
    x <- pad + (cc + 0.5) * cell
    y <- 1 - pad - (r + 0.5) * cell
    grid::grid.rect(x, y, cell, cell,
                    gp = grid::gpar(col = "#5BBFBF", fill = NA,
                                    lwd = px / 64))
    if (i %in% marked) {
      grid::grid.circle(x, y, r = cell * 0.32,
                        gp = grid::gpar(col = NA, fill = "#E0707B"))
    }
  }
}
if ("--icons" %in% commandArgs(trailingOnly = TRUE) ||
    !file.exists("app/icons/icon-512.png")) {
  draw_icon("app/icons/icon-512.png", 512, 0.14)
  draw_icon("app/icons/icon-192.png", 192, 0.14)
  draw_icon("app/icons/icon-180.png", 180, 0.14)
  draw_icon("app/icons/maskable-512.png", 512, 0.22)
}

message("App data updated.")
