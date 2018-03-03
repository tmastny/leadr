#' A tibble leaderboard for \code{caret} \code{train} objects
#'
#' This function (often called as \code{leadr::board}) updates and returns
#' the model leaderboard for the project.
#'
#' @param model model to add to the leaderboard. If no model is supplied
#' (the default \code{null}), \code{board} returns the leaderboard tibble
#' for the project.
#' @param path globally sets the path to save models and leaderboards. By
#' default, the path is the project directory found by \code{usethis::proj_get()}.
#' @param dir globally sets name of directory where models are saved.
#' This will be a subdirectory of the specified path. The default directory
#' is named \code{/models_one}. If no argument is supplied, the model will be saved
#' in the previously specified directory. See the example below.
#' @param save whether \code{board} should save the supplied model to \code{dir}. If
#' \code{FALSE} the model will not be saved, but will be added to the leaderboard.
#' @param quiet whether \code{board} should return the leaderboard tibble to the console.
#' By default \code{quiet = FALSE} means that the tibble prints to console.
#' \code{quiet = TRUE} is useful in a \code{.Rmd} environment, where you want to add
#' the model to the leaderboard without printing the tibble.
#'
#' @return \code{tibble} containing the most up-to-date leaderboard.
#'
#' @examples
#' # add caret model to leaderboard
#' # model saved to "/models_one"
#' model <- train(...)
#' leadr::board(model)
#'
#' # return tibble leaderboard
#' leadr::board()
#'
#' # save to different directory
#' ensemble <- train(...)
#' leadr::board(ensemble, dir = "ensembles_one")
#'
#' # board automatically saves to previous directory
#' # model saved to "/ensembles_one"
#' ensemble2 <- train(...)
#' leadr::board(ensembles2)
#'
#' @importFrom magrittr %>%
#' @export
board <- function(
  model = NULL, path = NULL, dir = NULL, save = TRUE, quiet = FALSE) {

  # have a peak argument that returns a tibble showing previous model's ranking
  default_dir = "/models_one"

  if (is.null(get_path()) || (!is.null(path))) {
    if (is.null(path)) {
      set_path(usethis::proj_get())
    } else {
      set_path(path)
    }
  }
  path = get_path()

  if (is.null(get_dir()) || (!is.null(dir))) {
    if (is.null(dir)) {
      set_dir(default_dir)
    } else {
      set_dir(dir)
    }
  }
  dir = get_dir()

  leadrboard <- new_leadrboard()
  leadrboard_path <- paste0(path, "/leadrboard.RDS")
  if (file.exists(leadrboard_path))
    leadrboard <- readRDS(leadrboard_path)

  if (!is.null(model)) {
    model_num = nrow(leadrboard) + 1
    leadrboard <- add_to(leadrboard, model, model_num, dir)
    saveRDS(leadrboard, leadrboard_path)

    if (save) {
      model_path = paste0(path, "/", dir, "/")
      if (!dir.exists(model_path))
        dir.create(model_path)

      saveRDS(model, paste0(model_path, "model", model_num, ".RDS"))
    }
  }

  if (quiet) return(invisible(leadrboard))

  leadrboard$num <- id(leadrboard$num)
  leadrboard
}

new_leadrboard <- function() {
  # add a directory column so you can filter by directory
  tibble::tibble(
    num = integer(),
    dir = character(),
    model = character(),
    accuracy = numeric(),
    cv = integer(),
    method = character(),
    tune = list(),
    seed = list(),
    resample = integer()
  )
}

add_to <- function(leadrboard, model, num, dir) {
  # when adding to leadrboard, if metric doesn't exist as a column,
  # add it

  new_row = list()
  new_row$num = num #removed id(), see if column conversion works.
  new_row$dir = dir

  if (inherits(model, "train")) {
    new_row$model = model$method
    new_row$accuracy = max(model$results$Accuracy)
    new_row$cv = model$control$number
    new_row$method = model$control$method
    new_row$tune = list(as.list(model$bestTune))
    new_row$seed = list(model$control$seeds)
  } else {
    stop("leadr only supports caret train objects (so far).")
  }

  matched <- purrr::map_lgl(leadrboard$seed, ~identical(list(.), new_row$seed))
  if (any(matched)) {
    new_row$resample <- leadrboard$resample[which(matched)][1]
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



