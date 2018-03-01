# package global
.globals <- new.env(parent = emptyenv())
.globals$path <- NULL

#' A tibble leaderboard for \code{caret} \code{train} objects
#'
#' This function (called as \code{leadr::board}) returns and possibly
#' updates the model leaderboard for the project.
#'
#' @param model model to add to the leaderboard. The default \code{null}
#' means \code{leadr::board} just returns the leaderboard tibble.
#' @param path globally sets the path to save models and leaderboards. By
#' default, the path is the project directory.
#' @param level name of directory that contains saved models and leaderboards.
#' This will be a subdirectory of the specified path. The default \code{level}
#' is \code{one} which gives a directory name \code{/level_one/}.
#'
#' @return \code{tibble} containing the most updated leaderboard.
#'
#' @examples
#' # add caret model to leaderboard
#' # model <- train(...)
#' leadr::board(model)
#'
#' # to return tibble leaderboard
#' leadr::board()
#'
#' @importFrom magrittr %>%
#' @export
board <- function(
  model = NULL, spec = NULL, path = NULL, level = 'one', save = TRUE) {

  if (is.null(get_path()) || (!is.null(path))) {
    if (is.null(path)) {
      .globals$path <- usethis::proj_get()
    } else {
      .globals$path <- path
    }
  }

  path = get_path()
  path = paste0(path, "/level_", level, "/")
  if (!dir.exists(path))
    dir.create(path)

  leadrboard <- new_leadrboard()
  leadrboard_path <- paste0(path, "leadrboard.RDS")
  if (file.exists(leadrboard_path))
    leadrboard <- readRDS(leadrboard_path)

  if (!is.null(model)) {
    model_num = nrow(leadrboard) + 1
    leadrboard <- add_to(leadrboard, model, spec, model_num)
    saveRDS(leadrboard, leadrboard_path)

    if (save) saveRDS(model, paste0(path, "model", model_num, ".RDS"))
  }

  leadrboard
}

new_leadrboard <- function() {
  tibble::tibble(
    num = integer(),
    model = character(),
    accuracy = numeric(),
    cv = integer(),
    method = character(),
    tune = list(),
    seed = list(),
    resample = integer()
  )
}

add_to <- function(leadrboard, model, spec, num) {
  # when adding to leadrboard, if metric doesn't exist as a column,
  # add it

  new_row = list()
  new_row$num = num

  if (inherits(model, "train")) {
    new_row$model = model$method
    new_row$accuracy = max(model$results$Accuracy)
    new_row$cv = model$control$number
    new_row$method = model$control$method
    new_row$tune = list(as.list(model$bestTune))
    new_row$seed = list(model$control$seeds)
  }

  matched <- purrr::map_lgl(leadrboard$seed, ~identical(., new_row$seed))
  if (any(matched)) {
    new_row$resample <- leadrboard$resample[which(matched)]
  } else {
    if (!(nrow(leadrboard) > 0)) {
      new_row$resample <- 1
    } else {
      new_row$resample <- max(leadrboard$resample) + 1
    }
  }


  # To arrange spec if supplied:
  # https://stackoverflow.com/questions/17031039/how-to-sort-a-character-vector-according-to-a-specific-order
  # https://stackoverflow.com/questions/27312311/sort-a-named-list-in-r

  leadrboard <- leadrboard %>%
    dplyr::bind_rows(new_row)

  leadrboard <- leadrboard %>%
    dplyr::arrange(desc(accuracy))
}

get_path <- function() {
  .globals$path
}

