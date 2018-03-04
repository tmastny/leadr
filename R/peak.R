#' Peak at leaderboard positions
#'
#' This function returns a tibble with showing the models ranked closest to the
#' supplied model. Usually chained with the magrittr pipe \code{\%>\%}.
#'
#' @param leadrboard leaderboard tibble returned by \code{\link{board}}
#' @param ... a number, numbers, or vector of numbers to that correspond to model
#' ids in the leaderboard. See \code{\link{last_model}} to easily specify model ids.
#' @param n the number of rows to return. By default, the tibble will return 10 rows.
#' \code{peak} will return a tibble with more than 20 rows, but the console
#' output is limited by \code{options(tibble.print_max)}, which has a default of 20.
#' @param how takes values of \code{c("centered", "above", "below")}. Determines
#' where to peak around the first number supplied to \code{...}.
#'
#' @return A subset of the leaderboard tibble. Given the supplied model ids,
#' \code{peak} tries to return a tibble of length \code{n}. If the maximum distance
#' between model ids are greater than \code{n}, \code{peak} will return the shortest
#' tibble that includes all the models. In supporting consoles, the supplied
#' model ids are highlighted.
#'
#' @examples
#' leadr::board() %>%
#'   peak(last_model())
#'
#' # peak at models 1 and 2
#' board() %>%
#'   peak(1, 2)
#'
#' board() %>%
#'   peak(c(1, 2))
#'
#' @export
peak <- function(leadrboard, ..., n = 10, how = "centered") {
  id <- c(...)
  if (any(id > nrow(leadrboard)) || any(id < 1) || (!is.numeric(id)))
    stop("model id is not valid.")

  # tibble, by default can't print more than twenty rows
  set_id(id)

  place <- 6
  if (how %in% c('above', 'below'))
    place <- list(below = 1, above = 10)[how][[1]]

  pos <- return_pos(leadrboard$id, id, n, place)
  leadrboard[pos$lower:pos$upper,]
}

return_pos <- function(models, id, window_size = 10, place) {
  row_id <- which(models == id[1])

  ## calculate disance ()
  if (length(id) > 1) {
    positions <- which(models %in% id)
    distance <- dist(positions)
    if (max(distance) > window_size)
      return(list(lower = min(positions), upper = max(positions)))
    row_id = ceiling(mean(c(min(positions), max(positions))))
  }

  lower = row_id - (place - 1)
  if (lower < 1)
    lower <- 1

  upper <- row_id + window_size - place
  if (upper > length(models))
    upper <- length(models)

  upper <- min(length(models), lower + window_size - 1)
  lower <- max(1, upper - (window_size - 1))


  list(lower = lower, upper = upper)
}

#' Get the index of the last model ran
#'
#' This function returns the \code{id} value from the leaderboard for the last
#' model ran. This function is intended to be used with \code{\link{peak}}.
#'
#' @param id used to indicate the number of models before the previously ran model.
#' The default \code{0} indicates the previously ran model. \code{k} would mean the
#' model ran \code{k} times before the previous model.
#'
#' @return Number from the leaderboard column \code{id} for the last model.
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
last_model <- function(id = 0) {
  load_path <- paste0(get_path(), "/leadrboard.RDS")
  nrow(readRDS(load_path)) - id
}



