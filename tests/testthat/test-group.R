context("group")

test_that("The two models from the first test should have the same resample number", {
  group <- leadr::board() %>%
    dplyr::filter(group == 1) %>%
    .$index

  expect_equal(group[[1]], group[[2]])
})



