#' Draw a bingo card
#'
#' Draws a card on the current graphics device using grid, in the style of
#' the original prototype: a script-style title above a grid with thin teal
#' lines. Labels are wrapped and shrunk to fit their cells.
#'
#' @param x A `bingo_card`.
#' @param title Title above the grid.
#' @param subtitle Line under the title. Defaults to the card's intended
#'   audience from [card_audience()], e.g. `"Child fans at EPCOT"`. Use `""`
#'   for no subtitle.
#' @param line_col,text_col Colors of the grid lines and text.
#' @param title_family,text_family Font families for the title and the
#'   squares. A script font installed on your system (e.g. `"Segoe Script"` on
#'   Windows) gives the prototype look when saving with [save_bingo_cards()].
#' @param free_label Label for the free space.
#' @param footer Print the card id (used for feedback) at the bottom?
#' @param newpage Start a new page before drawing?
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
#' @examples
#' card <- make_bingo_card(seed = 1)
#' plot(card)
plot.bingo_card <- function(x,
                            title = "Theme park bingo",
                            subtitle = NULL,
                            line_col = "#8FD6D6",
                            text_col = "#3B2A20",
                            title_family = "serif",
                            text_family = "sans",
                            free_label = "FREE",
                            footer = TRUE,
                            newpage = TRUE,
                            ...) {
  if (is.null(subtitle)) subtitle <- card_audience(x)
  if (newpage) grid::grid.newpage()

  page_w <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  page_h <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  side <- min(0.82 * page_w, 0.66 * page_h)
  cell <- side / x$size
  grid_bottom <- (page_h - side) * 0.42
  grid_top <- grid_bottom + side
  left <- (page_w - side) / 2
  inches <- function(v) grid::unit(v, "inches")

  # Title and subtitle
  title_size <- fit_fontsize(title, side, cell * 0.9, side * 7,
                             title_family, "italic")
  subtitle_size <- title_size * 0.35
  subtitle_y <- grid_top + cell * 0.1
  title_y <- subtitle_y
  if (nzchar(subtitle)) {
    grid::grid.text(
      subtitle, x = inches(page_w / 2), y = inches(subtitle_y), vjust = 0,
      gp = grid::gpar(fontsize = subtitle_size, fontfamily = text_family,
                      col = text_col)
    )
    title_y <- subtitle_y + subtitle_size / 72 * 1.6
  }
  grid::grid.text(
    title, x = inches(page_w / 2), y = inches(title_y), vjust = 0,
    gp = grid::gpar(fontsize = title_size, fontfamily = title_family,
                    fontface = "italic", col = text_col)
  )

  # Cells: fit every label, then use a shared size so the card looks uniform,
  # shrinking only the labels that don't fit at that size.
  labels <- x$grid
  is_free <- is.na(labels)
  labels[is_free] <- if (x$free_space) free_label else ""
  families <- ifelse(is_free, title_family, text_family)
  faces <- ifelse(is_free, "italic", "plain")
  box <- cell * 0.84
  fits <- lapply(seq_along(labels), function(i) {
    if (!nzchar(labels[i])) return(NULL)
    fit_label(labels[i], box, box, cell * 72 * 0.2, families[i], faces[i])
  })
  sizes <- vapply(fits[!is_free & nzchar(labels)], `[[`, numeric(1), "fontsize")
  shared_size <- if (length(sizes)) stats::quantile(sizes, 0.25, names = FALSE) else 12
  for (i in seq_along(labels)) {
    if (!nzchar(labels[i])) next
    if (fits[[i]]$fontsize > shared_size) {
      fits[[i]] <- fit_label(labels[i], box, box, shared_size, families[i],
                             faces[i])
    }
  }

  for (r in seq_len(x$size)) {
    for (cc in seq_len(x$size)) {
      i <- (cc - 1) * x$size + r
      cx <- left + (cc - 0.5) * cell
      cy <- grid_top - (r - 0.5) * cell
      grid::grid.rect(
        x = inches(cx), y = inches(cy), width = inches(cell),
        height = inches(cell),
        gp = grid::gpar(col = line_col, fill = NA, lwd = 1.5)
      )
      if (is.null(fits[[i]])) next
      grid::grid.text(
        fits[[i]]$text, x = inches(cx), y = inches(cy),
        gp = grid::gpar(fontsize = fits[[i]]$fontsize, fontfamily = families[i],
                        fontface = faces[i], col = text_col, lineheight = 1.1)
      )
    }
  }

  if (footer) {
    grid::grid.text(
      sprintf("card %s", x$card_id),
      x = inches(left + side), y = inches(grid_bottom - cell * 0.15),
      just = c("right", "top"),
      gp = grid::gpar(fontsize = 8, fontfamily = text_family,
                      col = grDevices::adjustcolor(text_col, 0.5))
    )
  }
  invisible(x)
}

#' @export
plot.bingo_card_set <- function(x, ...) {
  for (card in x) plot(card, ...)
  invisible(x)
}

#' Save bingo cards to a PDF
#'
#' Writes one card per page to a PDF, ready to print.
#'
#' @param cards A `bingo_card` or `bingo_card_set` (or list of cards).
#' @param file Output PDF path.
#' @param paper `"letter"` (8.5 x 11 in) or `"a4"`.
#' @param ... Styling arguments passed to [plot.bingo_card()].
#' @return `file`, invisibly.
#' @export
#' @examples
#' cards <- make_bingo_cards(2, mode = "fan", park = "MK", seed = 7)
#' save_bingo_cards(cards, tempfile(fileext = ".pdf"))
save_bingo_cards <- function(cards, file, paper = c("letter", "a4"), ...) {
  paper <- match.arg(paper)
  if (inherits(cards, "bingo_card")) cards <- list(cards)
  if (!all(vapply(cards, inherits, logical(1), "bingo_card"))) {
    stop("`cards` must be bingo cards from make_bingo_card(s)().", call. = FALSE)
  }
  dims <- switch(paper, letter = c(8.5, 11), a4 = c(8.27, 11.69))
  if (capabilities("cairo")) {
    grDevices::cairo_pdf(file, width = dims[1], height = dims[2], onefile = TRUE)
  } else {
    grDevices::pdf(file, width = dims[1], height = dims[2], onefile = TRUE)
  }
  on.exit(grDevices::dev.off())
  for (card in cards) plot(card, ...)
  invisible(file)
}

# Largest font size (<= max_size) at which a single line fits in w x h inches.
fit_fontsize <- function(text, w, h, max_size, family, face) {
  size <- max_size
  repeat {
    dims <- text_dims(text, size, family, face)
    if ((dims[1] <= w && dims[2] <= h) || size <= 4) return(size)
    size <- size * 0.92
  }
}

# Wrap and shrink a label so it fits in a w x h inch box. Prefers the largest
# font size, then the fewest lines.
fit_label <- function(text, w, h, max_size, family, face) {
  words <- strsplit(text, "\\s+")[[1]]
  wraps <- unique(vapply(rev(seq_len(nchar(text))), function(k) {
    paste(strwrap(text, width = k), collapse = "\n")
  }, character(1)))
  wraps <- wraps[order(lengths(strsplit(wraps, "\n")))]
  size <- max_size
  repeat {
    for (candidate in wraps) {
      dims <- text_dims(candidate, size, family, face, lineheight = 1.1)
      if (dims[1] <= w && dims[2] <= h) {
        return(list(text = candidate, fontsize = size))
      }
    }
    if (size <= 4) {
      return(list(text = paste(words, collapse = "\n"), fontsize = size))
    }
    size <- size * 0.92
  }
}

text_dims <- function(text, size, family, face, lineheight = 1.2) {
  g <- grid::textGrob(text, gp = grid::gpar(fontsize = size, fontfamily = family,
                                            fontface = face,
                                            lineheight = lineheight))
  c(grid::convertWidth(grid::grobWidth(g), "inches", valueOnly = TRUE),
    grid::convertHeight(grid::grobHeight(g), "inches", valueOnly = TRUE))
}
