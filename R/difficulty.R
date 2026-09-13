#' Estimate a card's odds of a bingo
#'
#' Simulates play by crossing off each square independently with probability
#' `p_crossed` (the free space is always crossed), and reports how often the
#' card gets at least one full row, column, or diagonal.
#'
#' @param card A `bingo_card`.
#' @param n_sim Number of simulated park days.
#' @return A list with `p_bingo` (probability of at least one bingo),
#'   `expected_crossed` (expected number of squares crossed, including the
#'   free space), and `mean_p_crossed` (average `p_crossed` of the drawn
#'   squares).
#' @export
#' @examples
#' card <- make_bingo_card(seed = 3)
#' bingo_odds(card)
bingo_odds <- function(card, n_sim = 10000) {
  if (!inherits(card, "bingo_card")) {
    stop("`card` must be a bingo_card.", call. = FALSE)
  }
  size <- card$size
  p <- rep(1, size^2)
  cells <- (card$squares$col - 1) * size + card$squares$row
  p[cells] <- card$squares$p_crossed

  crossed <- matrix(stats::runif(n_sim * size^2), n_sim) <
    matrix(p, n_sim, size^2, byrow = TRUE)
  index <- matrix(seq_len(size^2), size)
  lines <- c(
    split(index, row(index)),
    split(index, col(index)),
    list(diag(index), diag(index[, size:1]))
  )
  bingo <- Reduce(`|`, lapply(lines, function(l) {
    rowSums(crossed[, l, drop = FALSE]) == length(l)
  }))

  list(
    p_bingo = mean(bingo),
    expected_crossed = sum(p),
    mean_p_crossed = mean(card$squares$p_crossed)
  )
}

#' Record which squares a player crossed off
#'
#' Turns a played card into feedback rows, one per square, for
#' [update_difficulty()].
#'
#' @param card A `bingo_card`.
#' @param crossed Which squares were crossed off: a character vector of square
#'   ids or labels, or a logical matrix with the same dimensions as the card.
#' @return A data frame with columns `card_id`, `id`, and `crossed`.
#' @export
#' @examples
#' card <- make_bingo_card(seed = 1)
#' fb <- record_feedback(card, crossed = card$squares$id[1:10])
#' head(fb)
record_feedback <- function(card, crossed) {
  if (!inherits(card, "bingo_card")) {
    stop("`card` must be a bingo_card.", call. = FALSE)
  }
  sq <- card$squares
  hit <- if (is.logical(crossed) && is.matrix(crossed)) {
    if (!identical(dim(crossed), dim(card$grid))) {
      stop("A logical `crossed` matrix must match the card's dimensions.",
           call. = FALSE)
    }
    crossed[cbind(sq$row, sq$col)]
  } else if (is.character(crossed)) {
    unknown <- setdiff(crossed, c(sq$id, sq$text))
    if (length(unknown) > 0) {
      stop("Not on this card: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    sq$id %in% crossed | sq$text %in% crossed
  } else {
    stop("`crossed` must be square ids/labels or a logical matrix.",
         call. = FALSE)
  }
  data.frame(card_id = card$card_id, id = sq$id, crossed = as.logical(hit),
             stringsAsFactors = FALSE)
}

#' Update difficulty estimates from player feedback
#'
#' Refines each square's `p_crossed` with observed outcomes. The current
#' estimate is treated as worth `prior_weight + n_feedback` observations, so
#' early guesses move quickly with real data and well-tested estimates stay
#' stable:
#'
#' `p_new = (p_crossed * (prior_weight + n_feedback) + crossed) /
#'          (prior_weight + n_feedback + shown)`
#'
#' @param squares A squares table, e.g. from [bingo_squares()].
#' @param feedback A data frame with columns `id` and `crossed` (logical), one
#'   row per square per played card, e.g. rows bound from [record_feedback()].
#' @param prior_weight How many observations the initial guess is worth.
#' @return The squares table with updated `p_crossed` and `n_feedback`.
#' @export
#' @examples
#' squares <- bingo_squares()
#' card <- make_bingo_card(squares, seed = 1)
#' fb <- record_feedback(card, crossed = card$squares$id[1:12])
#' updated <- update_difficulty(squares, fb)
update_difficulty <- function(squares, feedback, prior_weight = 10) {
  squares <- validate_squares(squares)
  if (!all(c("id", "crossed") %in% names(feedback))) {
    stop("`feedback` needs columns `id` and `crossed`.", call. = FALSE)
  }
  feedback <- feedback[!is.na(feedback$crossed), , drop = FALSE]
  unknown <- setdiff(feedback$id, squares$id)
  if (length(unknown) > 0) {
    warning("Ignoring feedback for unknown square ids: ",
            paste(unknown, collapse = ", "), call. = FALSE)
    feedback <- feedback[feedback$id %in% squares$id, , drop = FALSE]
  }
  if (nrow(feedback) == 0) return(squares)

  shown <- tapply(feedback$crossed, feedback$id, length)
  hits <- tapply(as.logical(feedback$crossed), feedback$id, sum)
  i <- match(names(shown), squares$id)
  weight <- prior_weight + squares$n_feedback[i]
  squares$p_crossed[i] <- (squares$p_crossed[i] * weight + hits) /
    (weight + shown)
  squares$n_feedback[i] <- squares$n_feedback[i] + as.integer(shown)
  squares
}
