#' Fetch finished-card results from players
#'
#' Reads the results players shared from the phone app when they tapped
#' "Done playing", from the Google Sheet inbox described in
#' `backend/README.md`. Each row is one square on one card, ready for
#' [update_difficulty()]. Requires the curl package.
#'
#' By default, cards played for less than an hour are dropped, since cards
#' finished soon after being dealt usually weren't really played.
#'
#' @inheritParams fetch_suggestions
#' @param min_minutes Drop cards played for less than this many minutes. Use
#'   `0` to keep every card.
#' @return A data frame with one row per square per card: `card_id`,
#'   `submission_id`, `received_at`, `mode`, `age`, `park`, `difficulty`
#'   (the card's settings), `started_at`, `ended_at`, `minutes` (time played),
#'   `id` (the square id), and `crossed` (logical).
#' @seealso [update_difficulty()]
#' @export
#' @examples
#' \dontrun{
#' results <- fetch_results()
#' squares <- update_difficulty(bingo_squares(), results)
#' }
fetch_results <- function(endpoint = Sys.getenv("PARKBINGO_ENDPOINT"),
                          token = Sys.getenv("PARKBINGO_TOKEN"),
                          min_minutes = 60) {
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

#' Fetch daily app usage counts
#'
#' Reads how many times the phone app was opened, how many cards were dealt,
#' how many cards were printed, and how many different devices did any of
#' those each day (UTC), from the Google Sheet inbox described in
#' `backend/README.md`. Each page load of the published app counts as one
#' visit, and each dealt card counts once, so a player who works through ten
#' cards adds ten `cards`. Printed cards count when the print dialog opens.
#' Anything done without signal isn't counted. Requires the curl package.
#'
#' @inheritParams fetch_suggestions
#' @return A data frame with one row per day that had at least one open:
#'   `date` (a Date), `visits` (app opens), `cards` (cards dealt in the app),
#'   `printed` (cards printed), and `visitors` (different devices). Counts are
#'   integers; days from before a count was added have `0` for it.
#' @seealso [fetch_visitors()] for totals per device.
#' @export
#' @examples
#' \dontrun{
#' visits <- fetch_visits()
#' colSums(visits[c("visits", "cards", "printed")])
#' }
fetch_visits <- function(endpoint = Sys.getenv("PARKBINGO_ENDPOINT"),
                         token = Sys.getenv("PARKBINGO_TOKEN")) {
  parse_visits(inbox_request("list_visits", endpoint, token))
}

parse_visits <- function(json) {
  counts <- c("visits", "cards", "printed", "visitors")
  rows <- parse_inbox(json, "visits", c("date", counts))
  out <- data.frame(date = as.Date(rows$date))
  out[counts] <- lapply(rows[counts], as_count)
  out
}

#' Fetch usage totals per device
#'
#' Reads one row per device that has used the phone app, from the Google Sheet
#' inbox described in `backend/README.md`. Devices are identified by a random
#' id the app keeps on the phone, so this tells repeat visitors apart from new
#' ones without identifying anyone. The id isn't linked to suggestions or card
#' results. The same person counts as more than one device if they use more
#' than one phone or browser, clear the site's data, or (on iPhone) use the app
#' both in Safari and from the home screen. Requires the curl package.
#'
#' @inheritParams fetch_suggestions
#' @return A data frame with one row per device: `device_id`, `first_seen` and
#'   `last_seen` (Dates, UTC), `days` (how many different days it was used),
#'   and its total `visits`, `cards`, and `printed`.
#' @seealso [fetch_visits()] for daily counts, including visitors per day.
#' @export
#' @examples
#' \dontrun{
#' visitors <- fetch_visitors()
#' nrow(visitors)                 # different devices
#' mean(visitors$days > 1)        # share that came back on another day
#' table(visitors$first_seen)     # new devices per day
#' }
fetch_visitors <- function(endpoint = Sys.getenv("PARKBINGO_ENDPOINT"),
                           token = Sys.getenv("PARKBINGO_TOKEN")) {
  parse_visitors(inbox_request("list_visitors", endpoint, token))
}

parse_visitors <- function(json) {
  counts <- c("days", "visits", "cards", "printed")
  rows <- parse_inbox(json, "visitors",
                      c("device_id", "first_seen", "last_seen", counts))
  out <- data.frame(
    device_id = rows$device_id,
    first_seen = as.Date(rows$first_seen),
    last_seen = as.Date(rows$last_seen),
    stringsAsFactors = FALSE
  )
  out[counts] <- lapply(rows[counts], as_count)
  out
}

# Sheet counts arrive as text; blanks (columns added later) are 0.
as_count <- function(x) {
  n <- suppressWarnings(as.integer(x))
  n[is.na(n)] <- 0L
  n
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
