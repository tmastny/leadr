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

  board(model)

  expect_true(file.exists(here::here("leadrboard.RDS")))

  path_dir <- board() %>%
    filter(id == at_last()) %>%
    select(path, dir) %>%
    unlist(., use.names = FALSE)

  path_to_model <- file.path(path_dir[1], path_dir[2],
                             paste0("model", at_last(), ".RDS"))
  expect_true(file.exists(path_to_model))
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

  board(model)

  expect_true(file.exists(here::here("leadrboard.RDS")))

  path_dir <- board() %>%
    filter(id == at_last()) %>%
    select(path, dir) %>%
    unlist(., use.names = FALSE)

  path_to_model <- file.path(path_dir[1], path_dir[2],
                             paste0("model", at_last(), ".RDS"))
  expect_true(file.exists(path_to_model))
})


