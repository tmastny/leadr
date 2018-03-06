context("peak")

library(caret)

test_that("If the leaderboard has less than 2 entries, peak returns the whole board", {
  peaked <- leadr::board() %>% peak(at_last())
  expect_true(nrow(peaked) == 2)
})

test_that("at_last returns correct model id", {
  expect_equal(at_last(), 2)
})

test_that("Peaking returns 9 rows whereever model is ranked", {
  skip_if_not_installed('glmnet')

  control <- trainControl(
    method = "cv",
    number = 5,
    savePredictions = 'final',
    returnResamp = 'final',
    classProbs = TRUE
  )

  model_list <- purrr::map(
    rep("glmnet", 20),
    ~train(
      Species ~ .,
      data = iris,
      method = .,
      trControl = control
    )
  )
  purrr::map(model_list, leadr::board)
  expect_true(nrow(leadr::board()) == 22)

  max_pos <- which.max(leadr::board()$score)
  max_id <- leadr::board()$id[max_pos]

  max_peak <- leadr::board() %>% peak(max_id)
  expect_true(nrow(max_peak) == 10)

  min_pos <- which.min(leadr::board()$score)
  min_id <- leadr::board()$id[min_pos]

  min_peak <- leadr::board() %>% peak(min_id)
  expect_true(nrow(max_peak) == 10)
})

test_that("The peaked leaderboard returns all specified models", {
  peaked <- board() %>%
    peak(15)

  expect_true(any(peaked$id == 15))

  peaked <- board() %>%
    peak(2, 22)

  expect_true(any(peaked$id == 1))
  expect_true(any(peaked$id == 9))

  peaked <- board() %>%
    peak(c(19, 8))

  expect_true(any(peaked$id == 19))
  expect_true(any(peaked$id == 8))

  peaked <- board() %>%
    peak(1, 8, 19, 22)

  expect_true(any(peaked$id == 1))
  expect_true(any(peaked$id == 8))
  expect_true(any(peaked$id == 19))
  expect_true(any(peaked$id == 22))
})

test_that("peaking at min and max score returns the full table", {
  b <- board()
  id_min <- b[1,]$id
  id_max <- b[nrow(b),]$id

  peaked <- b %>% peak(id_min, id_max)
  expect_equal(nrow(peaked), nrow(b))
})

test_that("at_last returns correct model id", {
  expect_equal(at_last(), 22)
  expect_equal(at_last(2), c(22, 21))
  expect_equal(at_last(3), c(22, 21, 20))
})


