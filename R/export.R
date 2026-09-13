#' Export squares for the phone app
#'
#' Writes the squares table, plus the mode, age, and park vocabularies, as JSON
#' for the web app in `app/`. Parks are written as arrays of codes.
#'
#' @param path Output `.json` path.
#' @param squares A squares table, e.g. from [bingo_squares()].
#' @return `path`, invisibly.
#' @export
#' @examples
#' export_squares_json(tempfile(fileext = ".json"))
export_squares_json <- function(path, squares = bingo_squares()) {
  squares <- validate_squares(squares)
  vocab <- square_vocab()
  out <- list(
    format_version = jsonlite::unbox(1L),
    modes = vocab$modes,
    ages = vocab$ages,
    parks = data.frame(code = vocab$parks, label = park_label(vocab$parks)),
    squares = data.frame(
      id = squares$id,
      text = squares$text,
      mode = squares$mode,
      age = squares$age,
      park = I(lapply(split_parks(squares$park), I)),
      category = squares$category,
      p_crossed = squares$p_crossed,
      n_feedback = squares$n_feedback
    )
  )
  json <- jsonlite::toJSON(out, pretty = TRUE, digits = NA)
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
}
