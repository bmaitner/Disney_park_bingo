#' Filter squares for a card
#'
#' Selects the squares eligible for a card with the given mode, age, and park,
#' optionally restricted to a range of `p_crossed`.
#'
#' @param squares A squares table, e.g. from [bingo_squares()].
#' @param mode `"cynic"` or `"fan"` keep squares of that mode plus those tagged
#'   `"any"`; `"mixed"` keeps all modes.
#' @param age `"adult"` keeps all squares; `"child"` keeps only child-suitable
#'   squares.
#' @param park A park name or code (see [park_code()]). `"any"` keeps only
#'   squares that can happen anywhere; a specific park adds its own squares.
#' @param difficulty `"any"` (no filter) or a numeric `c(min, max)` range of
#'   `p_crossed` to keep. Named difficulty levels don't filter; see
#'   [make_bingo_card()].
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
  if (is.character(difficulty) && !identical(difficulty, "any")) {
    stop("filter_squares() takes difficulty = \"any\" or c(min, max). ",
         "Levels like \"hard\" shape the mix in make_bingo_card() instead.",
         call. = FALSE)
  }
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
#' @param difficulty How hard the card should be. Levels tilt the mix of
#'   squares drawn from the easiest, middle, and hardest thirds (by
#'   `p_crossed`) of the eligible squares, so every setting can make a card:
#'   `"any"` draws evenly (8/8/8 on a 5x5 card), `"easy"` favors easy squares
#'   (12/8/4), `"medium"` favors the middle (4/16/4), and `"hard"` favors hard
#'   squares (4/8/12). A numeric `c(min, max)` instead keeps only squares with
#'   `p_crossed` in that range.
#' @param size Number of rows (and columns). Defaults to 5.
#' @param free_space Put a free space in the center (odd sizes only)?
#' @param balance With `difficulty = "any"` or a numeric range, `TRUE` draws
#'   evenly across the easiest, middle, and hardest thirds so that cards made
#'   together are comparably hard; `FALSE` draws at random. Difficulty levels
#'   always use their tilted mix.
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

  level <- if (is.numeric(difficulty)) "any" else
    match.arg(difficulty, c("any", "easy", "medium", "hard"))
  range <- if (is.numeric(difficulty)) difficulty else "any"
  pool <- filter_squares(squares, mode = mode, age = age, park = park,
                         difficulty = range)
  n_needed <- size^2 - free_space
  if (nrow(pool) < n_needed) {
    stop(sprintf(
      "Only %d squares match mode = '%s', age = '%s', park = '%s', difficulty = %s; a %dx%d card needs %d. Relax a filter or add squares.",
      nrow(pool), mode, age, park, format_difficulty(difficulty), size, size,
      n_needed
    ), call. = FALSE)
  }

  weights <- ifelse(park != "any" & pool$park != "any", park_weight, 1)
  picked <- if (balance || level != "any") {
    sample_balanced(pool$p_crossed, n_needed, weights, difficulty_mix(level))
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
  cat(sprintf("<bingo_card %s> %s\n", x$card_id, card_audience(x)))
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
    cat(sprintf("  %s: %s\n", card$card_id, card_audience(card)))
  }
  invisible(x)
}

#' Describe a card's intended audience
#'
#' Builds a short description of who a card is for from the settings it was
#' made with, used as the default subtitle when printing.
#'
#' @param card A `bingo_card`.
#' @return A single string such as `"Adult fans at any park"` or
#'   `"Child cynics at Magic Kingdom"`.
#' @export
#' @examples
#' card_audience(make_bingo_card(mode = "fan", age = "child", park = "EPCOT"))
card_audience <- function(card) {
  if (!inherits(card, "bingo_card")) {
    stop("`card` must be a bingo_card.", call. = FALSE)
  }
  who <- switch(card$mode, cynic = "cynics", fan = "fans",
                mixed = "fans and cynics")
  where <- if (card$park == "any") "any park" else park_label(card$park)
  sprintf("%s %s at %s", if (card$age == "child") "Child" else "Adult", who,
          where)
}

difficulty_range <- function(difficulty) {
  if (identical(difficulty, "any")) return(c(0, 1))
  if (!is.numeric(difficulty) || length(difficulty) != 2 || anyNA(difficulty) ||
      difficulty[1] > difficulty[2]) {
    stop("A numeric `difficulty` must be c(min, max) of p_crossed.",
         call. = FALSE)
  }
  difficulty
}

# Relative share of squares drawn from the hardest, middle, and easiest thirds.
difficulty_mix <- function(level) {
  switch(level,
         any = c(1, 1, 1),
         easy = c(1, 2, 3),
         medium = c(1, 4, 1),
         hard = c(3, 2, 1))
}

format_difficulty <- function(difficulty) {
  if (is.numeric(difficulty)) {
    sprintf("[%s, %s]", difficulty[1], difficulty[2])
  } else {
    sprintf("'%s'", difficulty)
  }
}

# Draw n indices without replacement from thirds of the pool ordered by
# p_crossed (hardest first), in proportion to mix.
sample_balanced <- function(p, n, weights, mix = c(1, 1, 1)) {
  ord <- order(p, stats::runif(length(p)))
  k <- min(length(mix), length(ord))
  if (k < length(mix)) mix <- rep(1, k)
  bins <- split(ord, cut(seq_along(ord), k, labels = FALSE))
  quotas <- floor(n * mix / sum(mix))
  extra <- n - sum(quotas)
  if (extra > 0) {
    bump <- sample.int(k, extra)
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
