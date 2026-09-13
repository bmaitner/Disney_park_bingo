#' Fetch square suggestions from players
#'
#' Reads the suggestions players sent from the phone app, from the Google Sheet
#' inbox described in `backend/README.md`. Requires the curl package.
#'
#' @param endpoint The inbox's Apps Script web app URL. Defaults to the
#'   `PARKBINGO_ENDPOINT` environment variable.
#' @param token The private read token created by `createReadToken()` in the
#'   script. Defaults to the `PARKBINGO_TOKEN` environment variable.
#' @return A data frame with one row per suggestion: `received_at`,
#'   `submission_id`, `text`, `mode`, `age`, `park`, `note`, and `status` (the
#'   column you fill in while reviewing).
#' @seealso [add_squares()] to add the ones you like to the squares table.
#' @export
#' @examples
#' \dontrun{
#' suggestions <- fetch_suggestions()
#' suggestions[suggestions$status == "", ]
#' }
fetch_suggestions <- function(endpoint = Sys.getenv("PARKBINGO_ENDPOINT"),
                              token = Sys.getenv("PARKBINGO_TOKEN")) {
  parse_suggestions(inbox_request("list_suggestions", endpoint, token))
}

suggestion_columns <- c(
  "received_at", "submission_id", "text", "mode", "age", "park", "note", "status"
)

parse_suggestions <- function(json) {
  parse_inbox(json, "suggestions", suggestion_columns)
}

#' Add squares to the squares table
#'
#' Appends new squares, e.g. reviewed player suggestions, giving each the next
#' free id. The result is validated.
#'
#' @param squares A squares table, e.g. from [bingo_squares()].
#' @param new A data frame with columns `text`, `mode`, `age`, `park`, and
#'   `category`, and optionally `p_crossed` (starting difficulty guess) and
#'   `n_feedback`. Other columns, like those from [fetch_suggestions()], are
#'   ignored.
#' @param p_crossed Starting `p_crossed` for rows that don't have one.
#' @return The squares table with the new rows at the end.
#' @export
#' @examples
#' new <- data.frame(
#'   text = "Stroller with a license plate", mode = "any", age = "child",
#'   park = "any", category = "people"
#' )
#' tail(add_squares(bingo_squares(), new), 2)
add_squares <- function(squares, new, p_crossed = 0.5) {
  squares <- validate_squares(squares)
  needed <- c("text", "mode", "age", "park", "category")
  missing_cols <- setdiff(needed, names(new))
  if (length(missing_cols) > 0) {
    stop("`new` is missing columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  if (nrow(new) == 0) return(squares)

  numbers <- suppressWarnings(as.integer(sub("^sq", "", squares$id)))
  start <- max(c(0L, numbers), na.rm = TRUE)
  rows <- data.frame(
    id = sprintf("sq%04d", start + seq_len(nrow(new))),
    text = new$text,
    mode = new$mode,
    age = new$age,
    park = new$park,
    category = new$category,
    p_crossed = if ("p_crossed" %in% names(new)) new$p_crossed else p_crossed,
    n_feedback = if ("n_feedback" %in% names(new)) new$n_feedback else 0L,
    stringsAsFactors = FALSE
  )
  rows[setdiff(names(squares), names(rows))] <- NA
  validate_squares(rbind(squares, rows[names(squares)]))
}

