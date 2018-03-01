context("level one models")

library(caret)

test_that("Model is saved to directory", {
  skip_if_not_installed('randomForest')

  unlink("../../level_one/", recursive = TRUE)

  folds <- 5
  seeds <- caret_seed(k_fold = folds)

  control <- trainControl(
    method = "cv",
    number = folds,
    savePredictions = 'final',
    returnResamp = 'final',
    classProbs = TRUE,
    seeds = seeds
  )

  model <- train(
    Species ~ .,
    data = iris,
    method = 'rf',
    trControl = control
  )

  leadr::board(model)

  expect_true(file.exists('../../level_one/leadrboard.RDS'))
  expect_true(file.exists('../../level_one/model1.RDS'))
})

test_that("Next model", {
  folds <- 5
  seeds <- caret_seed(k_fold = folds)

  control <- trainControl(
    method = "cv",
    number = folds,
    savePredictions = 'final',
    returnResamp = 'final',
    classProbs = TRUE,
    seeds = seeds
  )

  model <- train(
    Species ~ .,
    data = iris,
    method = 'glmnet',
    trControl = control
  )

  leadr::board(model)

  expect_true(file.exists('../../level_one/leadrboard.RDS'))
  expect_true(file.exists('../../level_one/model1.RDS'))
})


