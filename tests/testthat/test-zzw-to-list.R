context("to_list")


test_that("to_list returns the correct models", {
  filtered_board <- board() %>%
    dplyr::filter(group == 1)

  model_list <- filtered_board %>% to_list()
  expect_identical(
    model_list[[1]]$control$seeds,
    filtered_board$seeds[[1]]
  )
  expect_identical(
    model_list[[2]]$control$seeds,
    filtered_board$seeds[[2]]
  )
})

test_that("to_list returns a warning if directory or model doesn't exist", {
  filtered_board <- board() %>%
    dplyr::filter(group == 1)

  filtered_board$id <- c(100, 101)
  filtered_board$dir <- c("models_one", "not_exist")

  expect_warning(to_list(filtered_board[1,]), "The file *")
  expect_warning(to_list(filtered_board[2,]), "The directory *")
})


