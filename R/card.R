#' Filter squares for a card
#'
#' Selects the squares eligible for a card with the given mode, age, park, and
#' difficulty.
#'
#' @param squares A squares table, e.g. from [bingo_squares()].
#' @param mode `"cynic"` or `"fan"` keep squares of that mode plus those tagged
#'   `"any"`; `"mixed"` keeps all modes.
#' @param age `"adult"` keeps all squares; `"child"` keeps only child-suitable
#'   squares.
#' @param park A park name or code (see [park_code()]). `"any"` keeps only
#'   squares that can happen anywhere; a specific park adds its own squares.
#' @param difficulty `"any"` (no filter), `"easy"` (`p_crossed >= 0.6`),
#'   `"medium"` (0.3-0.6), `"hard"` (`p_crossed < 0.3`), or a numeric
#'   `c(min, max)` range of `p_crossed`.
#' @return The filtered squares table.
#' @export
#' @examples
#' nrow(filter_squares(bingo_squares(), mode = "fan", age = "child", park = "EPCOT"))
filter_squares <- function(squares = bingo_squares(),
                           mode = c("cynic", "fan", "mixed"),
                           age = c("adult", "child"),
                           park = "any",
                           difficulty = "any") {
  mode <- match.arg(mode)
  age <- match.arg(age)
  park <- park_code(park)
  range <- difficulty_range(difficulty)

  keep_mode <- if (mode == "mixed") TRUE else squares$mode %in% c(mode, "any")
  keep_age <- if (age == "adult") TRUE else squares$age == "child"
  keep_park <- vapply(split_parks(squares$park),
                      function(p) any(p %in% c("any", park)), logical(1))
  keep_diff <- squares$p_crossed >= range[1] & squares$p_crossed <= range[2]

  out <- squares[keep_mode & keep_age & keep_park & keep_diff, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Make a bingo card
#'
#' Randomly draws squares matching the requested mode, age, park, and
#' difficulty, and lays them out on a square grid with an optional free space
#' in the center.
#'
#' @inheritParams filter_squares
#' @param size Number of rows (and columns). Defaults to 5.
#' @param free_space Put a free space in the center (odd sizes only)?
#' @param balance If `TRUE`, draw evenly across easy/medium/hard thirds of the
#'   eligible squares (by `p_crossed`) so that cards made together are
#'   comparably hard.
#' @param park_weight Relative chance of drawing a park-specific square versus
#'   a square that can happen anywhere, when `park` is a specific park.
#' @param seed Optional random seed for a reproducible card.
#' @return An object of class `bingo_card`: a list with `grid` (character
#'   matrix of labels, `NA` for the free space), `ids` (matrix of square ids),
#'   `squares` (the drawn rows of the squares table with `row` and `col`
#'   positions), and the settings used.
#' @seealso [make_bingo_cards()], [save_bingo_cards()], [bingo_odds()]
#' @export
#' @examples
#' card <- make_bingo_card(mode = "cynic", park = "Magic Kingdom", seed = 1)
#' card
make_bingo_card <- function(squares = bingo_squares(),
                            mode = c("cynic", "fan", "mixed"),
                            age = c("adult", "child"),
                            park = "any",
                            difficulty = "any",
                            size = 5,
                            free_space = TRUE,
                            balance = TRUE,
                            park_weight = 2,
                            seed = NULL) {
  mode <- match.arg(mode)
  age <- match.arg(age)
  park <- park_code(park)
  squares <- validate_squares(squares)
  if (!is.numeric(size) || length(size) != 1 || size < 1 || size %% 1 != 0) {
    stop("`size` must be a positive whole number.", call. = FALSE)
  }
  free_space <- isTRUE(free_space) && size %% 2 == 1
  if (!is.null(seed)) set.seed(seed)

  pool <- filter_squares(squares, mode = mode, age = age, park = park,
                         difficulty = difficulty)
  n_needed <- size^2 - free_space
  if (nrow(pool) < n_needed) {
    stop(sprintf(
      "Only %d squares match mode = '%s', age = '%s', park = '%s', difficulty = %s; a %dx%d card needs %d. Relax a filter or add squares.",
      nrow(pool), mode, age, park, format_difficulty(difficulty), size, size,
      n_needed
    ), call. = FALSE)
  }

  weights <- ifelse(park != "any" & pool$park != "any", park_weight, 1)
  picked <- if (balance) {
    sample_balanced(pool$p_crossed, n_needed, weights)
  } else {
    sample_weighted(seq_len(nrow(pool)), n_needed, weights)
  }
  drawn <- pool[picked, , drop = FALSE]

  cells <- seq_len(size^2)
  center <- (size^2 + 1) / 2
  if (free_space) cells <- setdiff(cells, center)
  cells <- cells[sample.int(length(cells))]
  drawn$row <- (cells - 1) %/% size + 1
  drawn$col <- (cells - 1) %% size + 1
  drawn <- drawn[order(drawn$row, drawn$col), , drop = FALSE]
  rownames(drawn) <- NULL

  grid <- matrix(NA_character_, size, size)
  ids <- matrix(NA_character_, size, size)
  grid[cbind(drawn$row, drawn$col)] <- drawn$text
  ids[cbind(drawn$row, drawn$col)] <- drawn$id

  structure(
    list(
      card_id = paste(sample(c(0:9, letters[1:6]), 6, replace = TRUE),
                      collapse = ""),
      grid = grid,
      ids = ids,
      squares = drawn,
      size = size,
      free_space = free_space,
      mode = mode,
      age = age,
      park = park,
      difficulty = difficulty
    ),
    class = "bingo_card"
  )
}

#' Make several bingo cards
#'
#' Makes `n` independent cards with the same settings, e.g. one per person in
#' a party.
#'
#' @param n Number of cards.
#' @param ... Arguments passed to [make_bingo_card()].
#' @param seed Optional random seed for a reproducible set of cards.
#' @return A list of `bingo_card` objects with class `bingo_card_set`.
#' @export
#' @examples
#' cards <- make_bingo_cards(4, mode = "fan", age = "child", park = "EPCOT", seed = 42)
#' cards
make_bingo_cards <- function(n, ..., seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  cards <- lapply(seq_len(n), function(i) make_bingo_card(...))
  structure(cards, class = "bingo_card_set")
}

#' @export
print.bingo_card <- function(x, width = 16, ...) {
  cat(sprintf("<bingo_card %s> %s | %s | %s\n", x$card_id, x$mode, x$age,
              park_label(x$park)))
  labels <- x$grid
  labels[is.na(labels)] <- if (x$free_space) "FREE" else ""
  wrapped <- lapply(labels, function(s) {
    lines <- strwrap(s, width = width)
    if (length(lines) == 0) lines <- ""
    substr(lines, 1, width)
  })
  n_lines <- max(lengths(wrapped))
  border <- paste0("+", paste(rep(strrep("-", width + 2), x$size),
                              collapse = "+"), "+\n")
  cat(border)
  for (r in seq_len(x$size)) {
    for (l in seq_len(n_lines)) {
      row_cells <- vapply(seq_len(x$size), function(cc) {
        lines <- wrapped[[(cc - 1) * x$size + r]]
        formatC(if (l <= length(lines)) lines[l] else "", width = width,
                flag = "-")
      }, character(1))
      cat("| ", paste(row_cells, collapse = " | "), " |\n", sep = "")
    }
    cat(border)
  }
  invisible(x)
}

#' @export
print.bingo_card_set <- function(x, ...) {
  cat(sprintf("<bingo_card_set> %d cards\n", length(x)))
  for (card in x) {
    cat(sprintf("  %s: %s | %s | %s\n", card$card_id, card$mode, card$age,
                park_label(card$park)))
  }
  invisible(x)
}

difficulty_range <- function(difficulty) {
  if (is.numeric(difficulty)) {
    if (length(difficulty) != 2 || anyNA(difficulty) ||
        difficulty[1] > difficulty[2]) {
      stop("A numeric `difficulty` must be c(min, max) of p_crossed.",
           call. = FALSE)
    }
    return(difficulty)
  }
  switch(
    match.arg(difficulty, c("any", "easy", "medium", "hard")),
    any = c(0, 1),
    easy = c(0.6, 1),
    medium = c(0.3, 0.6),
    hard = c(0, 0.3)
  )
}

format_difficulty <- function(difficulty) {
  if (is.numeric(difficulty)) {
    sprintf("[%s, %s]", difficulty[1], difficulty[2])
  } else {
    sprintf("'%s'", difficulty)
  }
}

# Draw n indices without replacement, spreading them evenly across thirds of
# the pool ordered by p_crossed.
sample_balanced <- function(p, n, weights, n_bins = 3) {
  ord <- order(p, stats::runif(length(p)))
  bins <- split(ord, cut(seq_along(ord), min(n_bins, length(ord)),
                         labels = FALSE))
  quotas <- rep(n %/% length(bins), length(bins))
  extra <- n %% length(bins)
  if (extra > 0) {
    bump <- sample.int(length(bins), extra)
    quotas[bump] <- quotas[bump] + 1
  }
  picked <- unlist(Map(function(idx, q) {
    sample_weighted(idx, min(q, length(idx)), weights[idx])
  }, bins, quotas), use.names = FALSE)
  shortfall <- n - length(picked)
  if (shortfall > 0) {
    rest <- setdiff(seq_along(p), picked)
    picked <- c(picked, sample_weighted(rest, shortfall, weights[rest]))
  }
  picked
}

sample_weighted <- function(idx, n, weights) {
  if (n == 0) return(integer())
  if (length(idx) == 1) return(idx)
  idx[sample.int(length(idx), n, prob = weights)]
}

park_label <- function(park) {
  labels <- c(
    any = "Any park", magic_kingdom = "Magic Kingdom", epcot = "EPCOT",
    hollywood_studios = "Hollywood Studios", animal_kingdom = "Animal Kingdom",
    disney_springs = "Disney Springs", typhoon_lagoon = "Typhoon Lagoon",
    blizzard_beach = "Blizzard Beach"
  )
  unname(labels[park])
}
