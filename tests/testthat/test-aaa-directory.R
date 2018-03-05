context("directory")

library(caret)

test_that("board can save models to a different directory", {
  directory = "model_test"

  model <- train(Species ~ ., data = iris, method = 'rf')
  board(model, dir = directory)

  dir_path = file.path(here::here(), directory)
  expect_true(dir.exists(dir_path))

  path_to_file <- file.path(dir_path, "model1.RDS")
  expect_true(file.exists(path_to_file))
})

test_that("board automatically saved to previous directory", {
  model <- train(Species ~ ., data = iris, method = 'rf')
  board(model)

  directory = "model_test"
  dir_path = file.path(here::here(), directory)
  path_to_file <- file.path(dir_path, "model2.RDS")
  expect_true(file.exists(path_to_file))

  unlink(dir_path, recursive = TRUE)
  unlink(file.path(here::here(), "leadrboard.RDS"))
})

test_that("board leaderboard can exist in subdirectory of root", {
  # clean up
  leadr:::set_path(NULL)
  leadr:::set_dir(NULL)

  new_sub <- "new_sub"
  new_path <- here::here(new_sub)
  dir.create(new_path)

  model <- train(Species ~ ., data = iris, method = 'rf')
  board(model, new_path)

  leadrboard_path <- file.path(new_path, "leadrboard.RDS")
  expect_true(file.exists(leadrboard_path))

  path_to_model <- file.path(new_path, "models_one", "model1.RDS")
  expect_true(file.exists(path_to_model))
})

test_that("board saves model to previous path", {
  model <- train(Species ~ ., data = iris, method = 'rf')
  board(model)

  new_sub <- "new_sub"
  new_path <- here::here(new_sub)
  path_to_model <- file.path(new_path, "models_one", "model2.RDS")
  expect_true(file.exists(path_to_model))
})

test_that("board can add new folder to new path", {
  new_dir = "new_dir"

  model <- train(Species ~ ., data = iris, method = 'rf')
  board(model, dir = new_dir)

  new_sub <- "new_sub"
  new_path <- here::here(new_sub)
  path_to_model <- file.path(new_path, new_dir, "model3.RDS")
  expect_true(file.exists(path_to_model))

  unlink(new_path, recursive = TRUE)
  leadr:::set_path(NULL)
  leadr:::set_dir(NULL)
})


