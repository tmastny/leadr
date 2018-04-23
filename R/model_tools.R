#' Convert the leaderboard tibble to a list of models
#'
#' Given a possibly filtered leaderboard tibble from \code{\link{board}},
#' \code{model_list} returns every model in a list. Each entry in the
#' list in named by the model method and id.
#'
#' @param leadrboard the leaderboard tibble, or a filtered verison of it
#' from \code{\link{board}}
#'
#' @return a named list of caret models (\code{train} objects)
#'
#' @examples
#' model_list <- board() %>%
#'   filter(group == 1) %>%
#'   model_list()
#'
#' @importFrom magrittr %>%
#' @export
model_list <- function(leadrboard) {
  model_locations <- list(
    leadrboard$path,
    leadrboard$dir,
    leadrboard$id
  )
  models <- purrr::pmap(model_locations, get_model)

  model_ids <- list(
    leadrboard$model,
    leadrboard$id
  )
  model_names <- purrr::pmap(model_ids, paste, sep = "_")
  names(models) <- model_names
  models
}


get_model <- function(path, dir, id) {
  model_dir <- file.path(path, dir)
  if (!dir.exists(model_dir)) {
    warning("The directory ", dir, " does not exist. Returning NA.")
    return(NA)
  }
  file_name <- paste0("model", id, ".RDS")
  file_path <- file.path(model_dir, file_name)
  if (!file.exists(file_path)) {
    warning("The file ", file_name, " does not exist in ", dir, ". Returning NA.")
    return(NA)
  }
  readRDS(file_path)
}

#' Build list of model meta-data
#'
#' Extracts the model meta-data from \code{\link{board}} into a list.
#' This data can be used to exactly reproduce the model. Can be passed
#' to \code{\link{run}}.
#'
#' @param leadrboard the leaderboard tibble, or a filtered verison of it
#' from \code{\link{board}}
#'
#' @return a named list of model meta-data that can be based to a function
#' that builds a caret \code{train} model
#'
#' @examples
#' parameters <- board() %>%
#'   filter(id == 1) %>%
#'   as_argument()
#'
#' run(modeler, data, parameters)
#'
#' @export
as_argument <- function(leadrboard) {
  leadrboard %>%
    select(method, num, index, seeds, method, model, tune) %>%
    as.list() %>%
    purrr::map(~.[[1]])
}

#' Run function taking the model meta-data
#'
#' Runs a function that takes a data to train on and a
#' list of parameters.
#'
#' @param modeler user created function that is a wrapper around
#' caret \code{train}
#'
#' @param data training data for the caret \code{train} function
#'
#' @param parameters parameters that go into the modeler wrapper. If
#' a previous model is being rerun, use \code{\link{as_argument}}
#' to extract model meta-data from the \code{\link{board}}
#'
#' @return returns the return value of the \code{modeler} function
#' supplied, which should be a caret \code{train} object.
#'
#' @examples
#' parameters <- board() %>%
#'   filter(id == 1) %>%
#'   as_argument()
#'
#' run(modeler, data, parameters)
#'
#' @export
run <- function(modeler, data, parameters) {
  do.call(modeler, c(list(data = data), parameters))
}

#' Wrapper around caret \code{train}
#'
#' This is a wrapper around caret \code{train} that accepts the
#' model meta-data from \code{\link{board}}. This function is used
#' to exactly reproduce models in the leader board, or run
#' new models.
#'
#' The parameters correspond to the arguments in caret's
#' \code{trainControl} and \code{train} functions.
#'
#' The source code for this function is also an example how to
#' create your own modeler function for your own needs.
#'
#' Note: you must manually load the caret package to use this function.
#' caret is not a dependency of leadr.
#'
#' @return a caret \code{train} object
#'
#' @examples
#' library(caret)
#' library(leadr)
#' modeler(iris, model = "rf")
#'
#' @export
modeler <- function(data, method = "cv", num = 5,
                    index = NULL, seeds = NA, model,
                    tune = NULL) {
  control <- trainControl(method = method, number = num,
                          savePredictions = 'final', index = index)
  train(
    Species ~ .,
    data = data,
    method = model,
    trControl = control,
    tuneGrid = tune
  )
}
