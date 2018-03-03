context("caret_seed")

test_that("caret_seeds of equal initial seed are equal", {
  list1 <- leadr::board() %>%
    dplyr::filter(num == 1) %>%
    .$seed

  list2 <- leadr::board() %>%
    dplyr::filter(num == 2) %>%
    .$seed

  expect_identical(list1, list2)
})
