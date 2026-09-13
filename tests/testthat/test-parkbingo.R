test_that("shipped squares table is valid", {
  squares <- bingo_squares()
  expect_true(nrow(squares) > 100)
  expect_false(anyDuplicated(squares$id) > 0)
  expect_true(all(squares$p_crossed >= 0 & squares$p_crossed <= 1))
})

test_that("every mode/age/park combination can fill a 5x5 card", {
  squares <- bingo_squares()
  for (mode in c("cynic", "fan")) {
    for (age in c("child", "adult")) {
      for (park in square_vocab()$parks) {
        pool <- filter_squares(squares, mode = mode, age = age, park = park)
        expect_gte(nrow(pool), 24, label = paste(mode, age, park))
      }
    }
  }
})

test_that("validation catches bad values", {
  squares <- bingo_squares()
  bad <- squares
  bad$mode[1] <- "grumpy"
  expect_error(validate_squares(bad), "mode")
  bad <- squares
  bad$park[1] <- "any;epcot"
  expect_error(validate_squares(bad), "park")
  bad <- squares
  bad$id[2] <- bad$id[1]
  expect_error(validate_squares(bad), "duplicated")
  expect_error(validate_squares(squares[, -1]), "missing columns")
})

test_that("park names are normalized", {
  expect_equal(park_code("MK"), "magic_kingdom")
  expect_equal(park_code("Disney's Hollywood Studios"), "hollywood_studios")
  expect_equal(park_code("EPCOT"), "epcot")
  for (code in square_vocab()$parks) expect_equal(park_code(code), code)
  expect_equal(park_code("Disney Springs"), "disney_springs")
  expect_error(park_code("Universal"), "Unknown park")
})

test_that("filters respect mode, age, and park", {
  squares <- bingo_squares()
  kid_fan_epcot <- filter_squares(squares, "fan", "child", "epcot")
  expect_true(all(kid_fan_epcot$age == "child"))
  expect_true(all(kid_fan_epcot$mode %in% c("fan", "any")))
  expect_true(all(grepl("any|epcot", kid_fan_epcot$park)))
  expect_false(any(filter_squares(squares, "cynic", park = "any")$park != "any"))
})

test_that("cards have the right shape and are reproducible", {
  card <- make_bingo_card(mode = "cynic", park = "Animal Kingdom", seed = 10)
  expect_s3_class(card, "bingo_card")
  expect_equal(dim(card$grid), c(5, 5))
  expect_true(is.na(card$grid[3, 3]))
  expect_equal(sum(!is.na(card$grid)), 24)
  expect_false(anyDuplicated(card$squares$id) > 0)
  expect_identical(card$grid, make_bingo_card(mode = "cynic",
                                              park = "Animal Kingdom",
                                              seed = 10)$grid)

  no_free <- make_bingo_card(size = 4, seed = 1)
  expect_equal(sum(!is.na(no_free$grid)), 16)
  expect_error(make_bingo_card(difficulty = c(0.99, 1)), "Only")
})

test_that("balanced sampling spreads difficulty", {
  card <- make_bingo_card(mode = "mixed", balance = TRUE, seed = 5)
  pool <- filter_squares(bingo_squares(), mode = "mixed")
  thirds <- stats::quantile(pool$p_crossed, c(1 / 3, 2 / 3))
  expect_gte(sum(card$squares$p_crossed <= thirds[1]), 6)
  expect_gte(sum(card$squares$p_crossed >= thirds[2]), 6)
})

test_that("difficulty levels tilt the mix and always make a card", {
  squares <- bingo_squares()
  mean_p <- function(level) {
    mean(vapply(1:30, function(s) {
      card <- make_bingo_card(squares, mode = "fan", age = "child",
                              park = "EPCOT", difficulty = level, seed = s)
      mean(card$squares$p_crossed)
    }, numeric(1)))
  }
  easy <- mean_p("easy")
  any <- mean_p("any")
  hard <- mean_p("hard")
  expect_gt(easy, any)
  expect_gt(any, hard)

  for (mode in c("cynic", "fan")) {
    for (age in c("child", "adult")) {
      for (level in c("easy", "medium", "hard")) {
        card <- make_bingo_card(squares, mode = mode, age = age,
                                difficulty = level, seed = 1)
        expect_equal(nrow(card$squares), 24)
      }
    }
  }

  ranged <- make_bingo_card(squares, mode = "mixed", difficulty = c(0.3, 0.9),
                            seed = 1)
  expect_true(all(ranged$squares$p_crossed >= 0.3 &
                    ranged$squares$p_crossed <= 0.9))
  expect_error(filter_squares(squares, difficulty = "hard"), "make_bingo_card")
})

test_that("bingo odds behave", {
  card <- make_bingo_card(seed = 2)
  easy <- card
  easy$squares$p_crossed <- 1
  expect_equal(bingo_odds(easy, n_sim = 100)$p_bingo, 1)
  hard <- card
  hard$squares$p_crossed <- 0
  expect_equal(bingo_odds(hard, n_sim = 100)$p_bingo, 0)
  odds <- bingo_odds(card, n_sim = 2000)
  expect_true(odds$p_bingo > 0 && odds$p_bingo < 1)
})

test_that("feedback updates difficulty", {
  squares <- bingo_squares()
  card <- make_bingo_card(squares, seed = 4)
  ids <- card$squares$id
  fb <- record_feedback(card, crossed = ids[1])
  expect_equal(nrow(fb), 24)
  expect_equal(sum(fb$crossed), 1)

  updated <- update_difficulty(squares, fb, prior_weight = 10)
  i <- match(ids[1], squares$id)
  expect_equal(updated$p_crossed[i], (squares$p_crossed[i] * 10 + 1) / 11)
  expect_equal(updated$n_feedback[i], 1L)
  j <- match(ids[2], squares$id)
  expect_lt(updated$p_crossed[j], squares$p_crossed[j])
  untouched <- setdiff(squares$id, ids)
  expect_equal(updated$p_crossed[squares$id %in% untouched],
               squares$p_crossed[squares$id %in% untouched])

  m <- matrix(FALSE, 5, 5)
  m[1, ] <- TRUE
  expect_equal(sum(record_feedback(card, m)$crossed), 5)
  expect_error(record_feedback(card, "not a square"), "Not on this card")
})

test_that("squares round-trip through csv and parquet", {
  squares <- bingo_squares()
  csv <- tempfile(fileext = ".csv.gz")
  write_squares(squares, csv)
  expect_equal(bingo_squares(csv), squares)
  skip_if_not_installed("nanoparquet")
  pq <- tempfile(fileext = ".parquet")
  write_squares(squares, pq)
  expect_equal(bingo_squares(pq), squares)
})

test_that("cards render to PDF", {
  pdf_file <- tempfile(fileext = ".pdf")
  cards <- make_bingo_cards(2, mode = "fan", park = "EPCOT", seed = 1)
  save_bingo_cards(cards, pdf_file)
  expect_gt(file.size(pdf_file), 1000)
  expect_output(print(cards[[1]]), "bingo_card")
})

test_that("card audience describes the settings", {
  expect_equal(card_audience(make_bingo_card(mode = "fan", seed = 1)),
               "Adult fans at any park")
  expect_equal(card_audience(make_bingo_card(mode = "fan", age = "child",
                                             park = "EPCOT", seed = 1)),
               "Child fans at EPCOT")
  expect_equal(card_audience(make_bingo_card(mode = "mixed", park = "MK",
                                             seed = 1)),
               "Adult fans and cynics at Magic Kingdom")
})

test_that("squares export to JSON for the app", {
  json <- tempfile(fileext = ".json")
  export_squares_json(json)
  out <- jsonlite::read_json(json, simplifyVector = TRUE)
  squares <- bingo_squares()
  expect_equal(out$squares$id, squares$id)
  expect_equal(out$squares$p_crossed, squares$p_crossed)
  expect_type(out$squares$park, "list")
  expect_equal(out$squares$park[[match("sq0109", squares$id)]],
               c("magic_kingdom", "epcot"))
  expect_equal(out$parks$code, square_vocab()$parks)
})

test_that("app/squares.json is in sync with the squares CSV", {
  app_json <- test_path("..", "..", "app", "squares.json")
  skip_if_not(file.exists(app_json), "app/ not available (e.g. in R CMD check)")
  fresh <- tempfile(fileext = ".json")
  export_squares_json(fresh)
  expect_identical(readLines(app_json), readLines(fresh),
                   info = "Run `Rscript tools/update_app.R` to refresh the app.")
})
