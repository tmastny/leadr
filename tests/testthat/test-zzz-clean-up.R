context("clean-up")

test_that("Clean-up successful", {
  #skip("save some models to work with")

  unlink(here::here("models"), recursive = TRUE)
  unlink(here::here("leadrboard.RDS"))

  expect_false(dir.exists(here::here("models")))
  expect_false(file.exists(here::here("leadrboard.RDS")))
})


