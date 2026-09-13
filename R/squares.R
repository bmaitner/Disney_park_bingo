#' Allowed values for the squares table
#'
#' Vocabularies used to validate the `mode`, `age`, and `park` columns of a
#' squares table.
#'
#' @return A named list of character vectors: `modes`, `ages`, and `parks`.
#' @export
#' @examples
#' square_vocab()$parks
square_vocab <- function() {
  list(
    modes = c("cynic", "fan", "any"),
    ages = c("child", "adult"),
    parks = c(
      "any", "magic_kingdom", "epcot", "hollywood_studios", "animal_kingdom",
      "disney_springs", "typhoon_lagoon", "blizzard_beach"
    )
  )
}

square_columns <- c(
  "id", "text", "mode", "age", "park", "category", "p_crossed", "n_feedback"
)

#' Bingo squares table
#'
#' Reads the table of candidate bingo squares. By default this is the table
#' shipped with the package (`inst/extdata/bingo_squares.csv`), but any CSV
#' (optionally gzipped) or Parquet file with the same columns can be used.
#'
#' @section Columns:
#' \describe{
#'   \item{id}{Stable unique identifier (e.g. `"sq0001"`). Used to link player
#'     feedback back to squares, so never reuse or renumber ids.}
#'   \item{text}{Label printed on the card. Keep it short (roughly 40
#'     characters or fewer).}
#'   \item{mode}{`"cynic"`, `"fan"`, or `"any"` (fits both).}
#'   \item{age}{`"child"` (suitable for everyone) or `"adult"` (adults only).}
#'   \item{park}{Where the square can occur: `"any"`, or one or more park codes
#'     separated by `;` (e.g. `"magic_kingdom;epcot"`). See [square_vocab()].}
#'   \item{category}{Free-form grouping such as `people`, `outfits`, `food`,
#'     `rides`.}
#'   \item{p_crossed}{Estimated proportion of players who cross the square off
#'     during a typical park day (0-1). Higher is easier; difficulty is
#'     `1 - p_crossed`. Starts as a guess and is refined with
#'     [update_difficulty()].}
#'   \item{n_feedback}{Number of player observations that have informed
#'     `p_crossed` (0 for an untested guess).}
#' }
#'
#' @param path Path to a `.csv`, `.csv.gz`, or `.parquet` file. `NULL` (the
#'   default) uses the table shipped with the package.
#' @return A data frame with the columns described below.
#' @seealso [write_squares()], [validate_squares()]
#' @export
#' @examples
#' squares <- bingo_squares()
#' table(squares$mode, squares$age)
bingo_squares <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "bingo_squares.csv", package = "parkbingo")
  }
  if (!file.exists(path)) {
    stop("Squares file not found: ", path, call. = FALSE)
  }
  squares <- if (is_parquet(path)) {
    check_nanoparquet()
    as.data.frame(nanoparquet::read_parquet(path))
  } else {
    utils::read.csv(
      path, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8"
    )
  }
  validate_squares(squares)
}

#' Write a squares table
#'
#' Validates and writes a squares table. The format is chosen from the file
#' extension: `.csv`, `.csv.gz`, or `.parquet` (gzip-compressed, requires the
#' nanoparquet package).
#'
#' @param squares A squares table, e.g. from [bingo_squares()].
#' @param path Output file path.
#' @return `path`, invisibly.
#' @export
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' write_squares(bingo_squares(), tmp)
write_squares <- function(squares, path) {
  squares <- validate_squares(squares)
  if (is_parquet(path)) {
    check_nanoparquet()
    nanoparquet::write_parquet(squares, path, compression = "gzip")
  } else {
    con <- if (grepl("\\.gz$", path)) gzfile(path, "w") else file(path, "w")
    on.exit(close(con))
    utils::write.csv(squares, con, row.names = FALSE, fileEncoding = "UTF-8")
  }
  invisible(path)
}

#' Validate a squares table
#'
#' Checks that a squares table has the required columns, unique ids, and
#' allowed values, and coerces column types. Extra columns are kept.
#'
#' @param squares A data frame.
#' @return The validated data frame (invisibly). Errors describe any problems.
#' @export
#' @examples
#' validate_squares(bingo_squares())
validate_squares <- function(squares) {
  if (!is.data.frame(squares)) {
    stop("`squares` must be a data frame.", call. = FALSE)
  }
  missing_cols <- setdiff(square_columns, names(squares))
  if (length(missing_cols) > 0) {
    stop("Squares table is missing columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  squares <- as.data.frame(squares, stringsAsFactors = FALSE)
  chr_cols <- c("id", "text", "mode", "age", "park", "category")
  squares[chr_cols] <- lapply(squares[chr_cols], function(x) trimws(as.character(x)))
  squares$p_crossed <- as.numeric(squares$p_crossed)
  squares$n_feedback <- as.integer(squares$n_feedback)

  vocab <- square_vocab()
  problems <- character()
  flag <- function(bad, what) {
    if (any(bad)) {
      rows <- which(bad)
      problems <<- c(problems, sprintf(
        "%s (rows %s)", what, paste(utils::head(rows, 10), collapse = ", ")
      ))
    }
  }

  flag(is.na(squares$id) | squares$id == "", "missing id")
  flag(duplicated(squares$id) & !is.na(squares$id), "duplicated id")
  flag(is.na(squares$text) | squares$text == "", "missing text")
  flag(!squares$mode %in% vocab$modes,
       paste0("mode not one of ", paste(vocab$modes, collapse = "/")))
  flag(!squares$age %in% vocab$ages,
       paste0("age not one of ", paste(vocab$ages, collapse = "/")))
  flag(!vapply(split_parks(squares$park), function(p) {
    length(p) > 0 && all(p %in% vocab$parks) && !("any" %in% p && length(p) > 1)
  }, logical(1)), "invalid park code (see square_vocab())")
  flag(is.na(squares$p_crossed) | squares$p_crossed < 0 | squares$p_crossed > 1,
       "p_crossed not between 0 and 1")
  flag(is.na(squares$n_feedback) | squares$n_feedback < 0,
       "n_feedback not a non-negative integer")

  if (length(problems) > 0) {
    stop("Invalid squares table:\n", paste0("  * ", problems, collapse = "\n"),
         call. = FALSE)
  }
  rownames(squares) <- NULL
  invisible(squares)
}

#' Normalize a park name
#'
#' Converts common park names and abbreviations (e.g. `"MK"`,
#' `"Magic Kingdom"`, `"DHS"`) to the codes used in the squares table.
#'
#' @param park A single park name, abbreviation, or code.
#' @return A park code from `square_vocab()$parks`.
#' @export
#' @examples
#' park_code("Hollywood Studios")
#' park_code("AK")
park_code <- function(park) {
  if (length(park) != 1 || is.na(park)) {
    stop("`park` must be a single park name.", call. = FALSE)
  }
  key <- sub("^disney['’]?s\\s+", "", tolower(park))
  key <- gsub("[^a-z]", "", key)
  aliases <- c(
    any = "any", all = "any",
    magickingdom = "magic_kingdom", mk = "magic_kingdom",
    epcot = "epcot",
    hollywoodstudios = "hollywood_studios", hs = "hollywood_studios",
    dhs = "hollywood_studios", mgm = "hollywood_studios",
    animalkingdom = "animal_kingdom", ak = "animal_kingdom",
    dak = "animal_kingdom",
    disneysprings = "disney_springs", springs = "disney_springs",
    ds = "disney_springs",
    typhoonlagoon = "typhoon_lagoon", tl = "typhoon_lagoon",
    blizzardbeach = "blizzard_beach", bb = "blizzard_beach"
  )
  code <- unname(aliases[key])
  if (is.na(code)) {
    stop("Unknown park '", park, "'. Use one of: ",
         paste(square_vocab()$parks, collapse = ", "), call. = FALSE)
  }
  code
}

split_parks <- function(park) {
  lapply(strsplit(ifelse(is.na(park), "", park), ";", fixed = TRUE), trimws)
}

is_parquet <- function(path) {
  grepl("\\.parquet$", path, ignore.case = TRUE)
}

check_nanoparquet <- function() {
  if (!requireNamespace("nanoparquet", quietly = TRUE)) {
    stop("Reading or writing Parquet files requires the 'nanoparquet' package.",
         call. = FALSE)
  }
}
