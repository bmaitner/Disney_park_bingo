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
  if (!nzchar(endpoint) || !nzchar(token)) {
    stop("Set PARKBINGO_ENDPOINT and PARKBINGO_TOKEN (see backend/README.md), ",
         "or pass `endpoint` and `token`.", call. = FALSE)
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("fetch_suggestions() requires the 'curl' package.", call. = FALSE)
  }
  # POST keeps the token out of URLs and logs. Apps Script answers with a
  # redirect that curl follows as a GET.
  handle <- curl::new_handle(followlocation = TRUE)
  curl::handle_setheaders(handle, "Content-Type" = "text/plain;charset=utf-8")
  curl::handle_setopt(handle, postfields = jsonlite::toJSON(
    list(type = "list_suggestions", token = token), auto_unbox = TRUE
  ))
  response <- curl::curl_fetch_memory(endpoint, handle = handle)
  if (response$status_code >= 400) {
    stop("Suggestion inbox returned HTTP ", response$status_code, call. = FALSE)
  }
  parse_suggestions(rawToChar(response$content))
}

suggestion_columns <- c(
  "received_at", "submission_id", "text", "mode", "age", "park", "note", "status"
)

parse_suggestions <- function(json) {
  result <- tryCatch(
    jsonlite::fromJSON(json, simplifyVector = TRUE),
    error = function(e) {
      stop("Suggestion inbox didn't return JSON. Check that PARKBINGO_ENDPOINT ",
           "is the web app URL ending in /exec.", call. = FALSE)
    }
  )
  if (!isTRUE(result$ok)) {
    stop("Suggestion inbox error: ", result$error %||% "unknown", call. = FALSE)
  }
  rows <- result$suggestions
  if (length(rows) == 0) {
    return(as.data.frame(
      stats::setNames(replicate(length(suggestion_columns), character(),
                                simplify = FALSE), suggestion_columns),
      stringsAsFactors = FALSE
    ))
  }
  rows <- as.data.frame(rows, stringsAsFactors = FALSE)
  rows[setdiff(suggestion_columns, names(rows))] <- ""
  rows[suggestion_columns]
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

`%||%` <- function(x, y) if (is.null(x)) y else x
