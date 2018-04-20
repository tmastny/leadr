context("directory")

library(caret)
library(dplyr)

test_that("board can save models to a different directory", {
  directory = "model_test"

  model <- train(Species ~ ., data = iris, method = 'rf')
  board(model, dir = directory)

  model_root <- board() %>%
    filter(id == at_last()) %>%
    .$path

  model_dir <- file.path(model_root, directory)
  expect_true(dir.exists(model_dir))

  path_to_model <- file.path(model_dir, "model1.RDS")
  expect_true(file.exists(path_to_model))
})

test_that("board can have model directory in non-root folder", {
  not_root <- here::here("not_root_dir")
  new_path <- file.path(not_root, "new_models")

  model <- train(Species ~ ., data = iris, method = 'rf')
  board(model, path = new_path)

  saved_path <- board() %>%
    filter(id == at_last()) %>%
    .$path
  expect_equal(saved_path, new_path)

  path_to_model <- file.path(new_path, "initial", "model2.RDS")
  expect_true(file.exists(path_to_model))

  unlink(not_root, recursive = TRUE)
})


