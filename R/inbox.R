#' Fetch finished-card results from players
#'
#' Reads the results players shared from the phone app when they tapped
#' "Done playing", from the Google Sheet inbox described in
#' `backend/README.md`. Each row is one square on one card, ready for
#' [update_difficulty()]. Requires the curl package.
#'
#' Cards finished within a few minutes of being dealt usually weren't really
#' played, so consider setting `min_minutes`.
#'
#' @inheritParams fetch_suggestions
#' @param min_minutes Drop cards played for less than this many minutes.
#' @return A data frame with one row per square per card: `card_id`,
#'   `submission_id`, `received_at`, `mode`, `age`, `park`, `difficulty`
#'   (the card's settings), `started_at`, `ended_at`, `minutes` (time played),
#'   `id` (the square id), and `crossed` (logical).
#' @seealso [update_difficulty()]
#' @export
#' @examples
#' \dontrun{
#' results <- fetch_results(min_minutes = 30)
#' squares <- update_difficulty(bingo_squares(), results)
#' }
fetch_results <- function(endpoint = Sys.getenv("PARKBINGO_ENDPOINT"),
                          token = Sys.getenv("PARKBINGO_TOKEN"),
                          min_minutes = 0) {
  parse_results(inbox_request("list_results", endpoint, token), min_minutes)
}

result_columns <- c(
  "received_at", "submission_id", "card_id", "mode", "age", "park", "difficulty",
  "started_at", "ended_at", "square_id", "crossed"
)

parse_results <- function(json, min_minutes = 0) {
  rows <- parse_inbox(json, "results", result_columns)
  started <- parse_timestamp(rows$started_at)
  ended <- parse_timestamp(rows$ended_at)
  out <- data.frame(
    card_id = rows$card_id,
    submission_id = rows$submission_id,
    received_at = rows$received_at,
    mode = rows$mode,
    age = rows$age,
    park = rows$park,
    difficulty = rows$difficulty,
    started_at = started,
    ended_at = ended,
    minutes = as.numeric(difftime(ended, started, units = "mins")),
    id = rows$square_id,
    crossed = toupper(rows$crossed) == "TRUE",
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$minutes) & out$minutes >= min_minutes, , drop = FALSE]
  rownames(out) <- NULL
  out
}

parse_timestamp <- function(x) {
  as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
}

# POST keeps the token out of URLs and logs. Apps Script answers with a
# redirect that curl follows as a GET. Returns the response body as text.
inbox_request <- function(type, endpoint, token) {
  if (!nzchar(endpoint) || !nzchar(token)) {
    stop("Set PARKBINGO_ENDPOINT and PARKBINGO_TOKEN (see backend/README.md), ",
         "or pass `endpoint` and `token`.", call. = FALSE)
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Reading the inbox requires the 'curl' package.", call. = FALSE)
  }
  handle <- curl::new_handle(followlocation = TRUE)
  curl::handle_setheaders(handle, "Content-Type" = "text/plain;charset=utf-8")
  curl::handle_setopt(handle, postfields = jsonlite::toJSON(
    list(type = type, token = token), auto_unbox = TRUE
  ))
  response <- curl::curl_fetch_memory(endpoint, handle = handle)
  if (response$status_code >= 400) {
    stop("Inbox returned HTTP ", response$status_code, call. = FALSE)
  }
  rawToChar(response$content)
}

parse_inbox <- function(json, kind, columns) {
  result <- tryCatch(
    jsonlite::fromJSON(json, simplifyVector = TRUE),
    error = function(e) {
      stop("Inbox didn't return JSON. Check that PARKBINGO_ENDPOINT ",
           "is the web app URL ending in /exec.", call. = FALSE)
    }
  )
  if (!isTRUE(result$ok)) {
    error <- if (is.null(result$error)) "unknown" else result$error
    if (identical(error, "unknown request type")) {
      error <- paste0(error, " (redeploy the latest backend/inbox.gs)")
    }
    stop("Inbox error: ", error, call. = FALSE)
  }
  rows <- result[[kind]]
  if (length(rows) == 0) {
    empty <- stats::setNames(
      replicate(length(columns), character(), simplify = FALSE), columns
    )
    return(as.data.frame(empty, stringsAsFactors = FALSE))
  }
  rows <- as.data.frame(rows, stringsAsFactors = FALSE)
  rows[setdiff(columns, names(rows))] <- ""
  rows[columns]
}
