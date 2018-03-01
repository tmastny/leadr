#' Peak at the leaderboard position of a model
#'
#' This function returns a tibble with the 8 models ranked closest to the
#' supplied model in accuracy. Usually chained with the magrittr pipe \code{\%>\%}.
#'
#' @param leadrboard leadrboard tibble returned by \code{\link{board}}
#' @param num the number of the model. See \code{\link{last_model}} to easily
#' specify model number.
#'
#' @return A filtered leadrboard tibble with only 9 rows, centerd around the
#' supplied model number.
#'
#' @examples
#' leadr::board() %>%
#'   peak(last_model())
#'
#' @export
peak <- function(leadrboard, num) {
  pos <- return_pos(leadrboard$num, num)
  leadrboard[pos$lower:pos$upper,]
}

return_pos <- function(models, num) {
  row_num <- which(models == num)

  lower = row_num - 4
  if (lower < 1)
    lower <- 1

  upper <- row_num + 4
  if (upper > length(models))
    upper <- length(models)

  upper <- min(length(models), lower + 8)
  lower <- max(1, upper - 8)

  list(lower = lower, upper = upper)
}

#' Get the index of the last model ran
#'
#' This function returns the leaderboard column \code{num} for the last
#' model ran. This function is intended to be used with \code{\link{peak}}.
#'
#' @param num used to indicate the number of models before the previously ran model.
#' The default \code{0} indicates the previously ran model. \code{k} would mean the
#' model ran \code{k} times before the previous model.
#' @param level specifies the directory of the leaderboard. By default the level
#' is \code{'one'}.
#'
#' @return Number from the leaderboard column \code{num} for the last model.
#'
#' @examples
#' # peak at previous model
#' leadr::board() %>%
#'   peak(last_model())
#'
#' # peak at model before previous
#' leadr::board() %>%
#'   peak(last_model(1))
#'
#' @export
last_model <- function(num = 0, level = 'one') {
  path <- get_path()
  path <- paste0(path, "/level_", level, "/leadrboard.RDS")
  nrow(readRDS(path)) - num
}



