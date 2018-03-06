context("metrics")

library(caret)

test_that("board sorts and groups new metrics to the top", {
  model_list <- purrr::map(
    rep("rf", 5),
    ~train(
      mpg ~ .,
      data = mtcars,
      method = .
    )
  )
  purrr::map(model_list, board)
  top5 <- 1:5
  b <- board()

  expect_equal(unique(b[top5, 'metric'])[[1]], model_list[[1]]$metric)
  expect_identical(
    b[top5, 'score'][[1]],
    b[top5, 'score'][[1]][order(b[top5, 'score'][[1]], decreasing = FALSE)]
  )
})

