context("board")

library(caret)

test_that("Model is saved to directory", {
  skip_if_not_installed('randomForest')

  unlink(here::here("models_one"), recursive = TRUE)
  unlink(here::here("leadrboard.RDS"))

  folds <- 5
  seeds <- caret_seed(number = folds)

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

  expect_true(file.exists(here::here("leadrboard.RDS")))
  expect_true(file.exists(here::here("models_one", "model1.RDS")))
})

test_that("Next model", {
  folds <- 5
  seeds <- caret_seed(number = folds)

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

  expect_true(file.exists(here::here("leadrboard.RDS")))
  expect_true(file.exists(here::here("models_one", "model2.RDS")))
})


