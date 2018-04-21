#' Return out of fold model predictions
#'
#' Given a model or list of models, this function returns the
#' out of fold predictions. These out of fold predictions can be used
#' to make stacked or blended ensembles. See the ensemble
#' \href{https://tmastny.github.io/leadr/articles/ensemble.html}{vignette}
#' for examples.
#'
#' @param models A model or list of models to get the predictions
#' @param type the results of the prediction. For classification models,
#' \code{"raw"} returns the outcome label and \code{"prob"} returns the
#' label probabilities.
#'
#' @return a tibble with one column per model and a column of the training data
#' outcomes. If \code{type = "prob"} there will be n columns per model, where
#' n is the number of labels in the outcome.
#'
#' @examples
#' oofs <- oof_grab(models)
#'
#' @importFrom magrittr %>%
#' @export
oof_grab <- function(models, type = "raw") {
  if (inherits(models, "train")) models <- list(models)

  agg_data <- purrr::map_dfc(models, grabber, type)
  agg_data <- agg_data %>% add_observed(models[[1]])
  agg_data
}

grabber <- function(model, type) {

  if (is.null(model$pred)) {
    stop("Out of fold predictions were not saved in the caret model. ",
         "Re-run with savePredictions = 'final' or TRUE in trainControl.")
  }
  grabbers <- list(raw = pred_grabber, prob = prob_grabber)
  grab <- grabbers[[type]]

  if (is.null(grab)) stop("Not a valid type. Use raw or prob.")

  pred_data <- tune_filter(model)
  grab(pred_data, model)
}

prob_grabber <- function(data, model) {
  columns <- as.character(unique(model$trainingData$.outcome))
  if (all(!columns %in% names(data))) {
    stop("Probabilities were not saved, or are not available in the caret model. ",
         "Re-run with classProbs = TRUE in trainControl.")
  }
  tibble::as_tibble(data[orderer(data), columns])
}

pred_grabber <- function(data, model) {
  tibble::as_tibble(data$pred[orderer(data)])
}

orderer <- function(data) {
  order(data$rowIndex)
}

tune_filter <- function(model) {
  col_names <- names(model$bestTune)
  col_values <- model$bestTune
  filtered_pred <- model$pred %>%
    dplyr::filter(
      !!!purrr::map2(
        col_names, col_values,
        ~rlang::quo(!!rlang::sym(.x) == !!.y)
        )
      )
}

add_observed <- function(agg_data, model) {
  outcome <- attr(model$terms, "variables")[[2]]

  data <- tune_filter(model)
  observed <- data$obs[orderer(data)]
  agg_data <- agg_data %>%
    tibble::add_column(!!outcome := observed)
}

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
