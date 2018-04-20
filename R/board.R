#' A tibble leaderboard for \code{caret} \code{train} objects
#'
#' This function updates and returns
#' the model leaderboard for the project. Please read the
#' \href{https://github.com/tmastny/leadr}{README} and the
#' introduction
#' \href{https://tmastny.github.io/leadr/articles/introduction.html}{vignette}.
#'
#'
#' @param model model to add to the leaderboard. If no model is supplied
#' (the default \code{null}), \code{board} returns the leaderboard tibble
#' for the project.
#' @param path globally sets the path to save models and leaderboards. By
#' default, the path is the project directory found by. For best results, the path
#' string should be constructed with \code{file.path} or
#' \href{https://github.com/krlmlr/here}{\code{here::here()}}.
#' @param dir globally sets name of directory where models are saved.
#' This will be a subdirectory of the specified path. The default directory
#' is named \code{models_one}. If no argument is supplied, the model will be saved
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
#' # model saved to "models_one"
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
#' # model saved to "ensembles_one"
#' ensemble2 <- train(...)
#' leadr::board(ensembles2)
#'
#' @importFrom magrittr %>%
#' @export
board <- function(
  model = NULL, path = here::here("models"), dir = "initial", save = TRUE,
  quiet = FALSE) {

  leadrboard <- new_leadrboard()
  leadrboard_path <- here::here("leadrboard.RDS")
  if (file.exists(leadrboard_path))
    leadrboard <- readRDS(leadrboard_path)

  if (!is.null(model)) {
    model_id = nrow(leadrboard) + 1
    leadrboard <- add_to(leadrboard, model, model_id, dir, path)
    saveRDS(leadrboard, leadrboard_path)

    if (save) {
      model_path = file.path(path, dir)
      if (!dir.exists(model_path))
        dir.create(model_path, recursive = TRUE)

      saveRDS(model, file.path(model_path, paste0("model", model_id, ".RDS")))
    }
  }

  if (quiet) return(invisible(leadrboard))

  leadrboard$id <- id(leadrboard$id)
  leadrboard
}

new_leadrboard <- function() {
  tibble::tibble(
    rank = integer(),
    id = integer(),
    dir = character(),
    model = character(),
    metric = character(),
    score = numeric(),
    public = numeric(),
    method = character(),
    num = integer(),
    group = integer(),
    index = list(),
    tune = list(),
    seeds = list(),
    path = character()
  )
}

add_to <- function(leadrboard, model, id, dir, path) {
  new_row = list()
  new_row$rank = 1
  new_row$id = id
  new_row$dir = dir

  if (inherits(model, "train")) {
    new_row$model = model$method
    new_row$metric = model$metric
    new_row$score = max(model$results[,model$metric])
    new_row$method = model$control$method
    new_row$num = model$control$number
    new_row$group = NA
    new_row$index = list(model$control$index)
    new_row$tune = list(as.list(model$bestTune))
    new_row$seeds = list(model$control$seeds)
    new_row$path = path
  } else {
    stop("leadr only supports caret train objects (so far).")
  }

  matched <- purrr::map_lgl(leadrboard$index, ~identical(list(.), new_row$index))
  if (any(matched)) {
    new_row$group <- leadrboard$group[which(matched)][1]
  } else {
    if (!(nrow(leadrboard) > 0)) {
      new_row$group <- 1
    } else {
      new_row$group <- max(leadrboard$group) + 1
    }
  }

  leadrboard <- leadrboard %>%
    dplyr::bind_rows(new_row)

  direction <- dplyr::desc
  if (model$metric == "RMSE") {
    direction <- function(x) x
  }

  metric_types <- unique(leadrboard$metric)
  current_pos <- which(metric_types == model$metric)
  new_order <- c(metric_types[current_pos], metric_types[-current_pos])

  leadrboard <- leadrboard %>%
    dplyr::mutate(metric = factor(metric, levels = new_order)) %>%
    dplyr::arrange(metric) %>%
    dplyr::group_by(metric) %>%
    dplyr::arrange(direction(score), .by_group = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(metric = as.character(metric))

  leadrboard <- leadrboard %>%
    dplyr::group_by(metric) %>%
    dplyr::mutate(rank = row_number()) %>%
    dplyr::group_by(score) %>%
    dplyr::mutate(rank = min(rank)) %>%
    dplyr::ungroup()
}

