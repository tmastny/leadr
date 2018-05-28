context("clean-up")

test_that("Clean-up successful", {
  #skip("save some models to work with")

  unlink(file.path(getwd(), "models"), recursive = TRUE)
  unlink(file.path(getwd(), "leadrboard.RDS"))

  expect_false(dir.exists(file.path(getwd(), "models")))
  expect_false(file.exists(file.path(getwd(), "leadrboard.RDS")))
})


