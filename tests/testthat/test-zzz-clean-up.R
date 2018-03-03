context("clean-up")

library(caret)

test_that("Clean-up successful", {
  unlink("../../models_one/", recursive = TRUE)
  unlink("../../leadrboard.RDS")

  expect_false(file.exists("../../models_one/"))
  expect_false(file.exists("../../leadrboard.RDS"))
})


