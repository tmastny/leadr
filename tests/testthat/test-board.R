context("board")

library(caret)

test_that("Model is saved to directory", {
  skip_if_not_installed('randomForest')

  unlink(here::here("models_one"), recursive = TRUE)
  unlink(here::here("leadrboard.RDS"))

  control <- trainControl(
    method = "cv",
    number = 5,
    savePredictions = 'final',
    returnResamp = 'final',
    classProbs = TRUE
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

  index <- board()$index[[1]]
  control <- trainControl(
    method = "cv",
    index = index,
    savePredictions = 'final',
    returnResamp = 'final',
    classProbs = TRUE
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


