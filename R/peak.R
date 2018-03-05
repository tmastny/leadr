#' Peak at leaderboard positions
#'
#' This function returns a tibble with showing the models ranked closest to the
#' supplied model. Usually chained with the magrittr pipe \code{\%>\%}.
#'
#' @param leadrboard leaderboard tibble returned by \code{\link{board}}
#' @param ... a number, numbers, or vector of numbers to that correspond to model
#' ids in the leaderboard. See \code{\link{at_last}} to easily specify model ids.
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
#' # peak at last model saved
#' board() %>%
#'   peak(at_last())
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

  set_id(id)

  place <- 6
  if (how %in% c('above', 'below'))
    place <- list(below = 1, above = 10)[how][[1]]

  pos <- return_pos(leadrboard$id, id, n, place)
  leadrboard[pos$lower:pos$upper,]
}

return_pos <- function(models, id, window_size = 10, place) {
  row_id <- which(models == id[1])

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

#' Get the id of the last model ran
#'
#' This function returns the model ids for the last \code{number} models saved
#' to leaderboard. This function is intended to be used with \code{\link{peak}}.
#'
#' @param number number that indicates the last n models to peak at.
#'
#' @return model id of last models
#'
#' @examples
#' # peak at previous model
#' board() %>%
#'   peak(at_last())
#'
#' # peak at last two models
#' board() %>%
#'   peak(at_last(2))
#'
#' # peak at last three models
#' # and model 1
#' board() %>%
#'   peak(at_last(3), 1)
#'
#' board() %>%
#'   dplyr::filter(id %in% at_last(3))
#'
#' @export
at_last <- function(number = 1) {
  number <- 1:number
  load_path <- file.path(get_path(), "leadrboard.RDS")
  nrow(readRDS(load_path)) - number + 1
}



