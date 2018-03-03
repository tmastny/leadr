context("peak")

library(caret)

test_that("If the leaderboard has less than 2 entries, peak returns the whole board", {
  peaked <- leadr::board() %>% peak(last_model())
  expect_true(nrow(peaked) == 2)
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

  max_pos <- which.max(leadr::board()$accuracy)
  max_id <- leadr::board()$id[max_pos]

  max_peak <- leadr::board() %>% peak(max_id)
  expect_true(nrow(max_peak) == 9)

  min_pos <- which.min(leadr::board()$accuracy)
  min_id <- leadr::board()$id[min_pos]

  min_peak <- leadr::board() %>% peak(min_id)
  expect_true(nrow(max_peak) == 9)
})





