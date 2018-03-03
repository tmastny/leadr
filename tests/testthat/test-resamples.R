context("resample")

test_that("The two models from the first test should have the same resample number", {
  resample1 <- leadr::board() %>%
    dplyr::filter(num == 1) %>%
    .$resample

  resample2 <- leadr::board() %>%
    dplyr::filter(num == 2) %>%
    .$resample

  expect_equal(resample1, resample2)
})



