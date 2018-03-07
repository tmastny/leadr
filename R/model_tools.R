#' Set seed for \code{caret}'s \code{trainControl}
#'
#' Given a starting seed and number of folds (or resamples),
#' this function creates a vector of seeds needed for the `caret`
#' trainControl object.
#'
#' @param seed starting seed for list of seeds
#' @param number number of folds or resamples
#'
#' @return List of seeds used by \code{trainControl}.
#'
#' @examples
#' trainControl(method = 'cv', number = 10, seeds = caret_seed(1234, 10))
#'
#' @export
caret_seed <- function(seed = 1, number) {
  set.seed(seed)
  seeds <- vector(mode = "list", length = number + 1)

  for(i in 1:number) seeds[[i]] <- sample.int(n=1000, 4)
  seeds[[number + 1]] <- sample.int(1000, 1)
  seeds
}

#' Return out of fold model predictions
#'
#' Given a model or list of models, this function returns the
#' out of fold predictions. These out of fold predictions can be used
#' to make stacked or blended ensembles. See this vignette for more
#' details.
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
#' oofs <- oof_grab(model_list)
#'
#' @importFrom magrittr %>%
#' @export
oof_grab <- function(models, type = "raw") {
  if (inherits(models, "train")) models <- list(models)

  agg_data <- purrr::map_dfc(models, grabber, type)
  agg_data %>% add_observed(models[[1]])
}

grabber <- function(model, type) {

  if (is.null(model$pred)) {
    stop("Out of fold predictions were not saved in the caret model. ",
         "Re-run with savePredictions = 'final' or TRUE in trainControl.")
  }
  grabbers <- list(raw = pred_grabber, prob = prob_grabber)
  grab <- grabbers[[type]]

  if (is.null(grab)) stop("Not a valid type. Use raw or prob.")

  pred_data <- save_filter(model)
  grab(pred_data, model)
}

# need to add warning if probabilities weren't saved
prob_grabber <- function(data, model) {
  columns <- as.character(unique(model$trainingData$.outcome))
  if (all(!columns %in% names(data))) {
    stop("Probabilities were not saved, or are not available in the caret model. ",
         "Re-run with classProbs = TRUE in trainControl.")
  }
  tibble::as_tibble(data[orderer(data), columns])
}

pred_grabber <- function(data, model) {
  # if (nrow(model$pred) != nrow(model$trainingData)) {
  #   stop("All resamples were saved, so oof_grab doesn't know which to pick in",
  #        "the caret model.", "Re-run with savePredictions = 'final' in trainControl.")
  # }
  tibble::as_tibble(data$pred[orderer(data)])
}

orderer <- function(data) {
  order(data$rowIndex)
}

save_filter <- function(model) {
  if (nrow(model$pred) != nrow(model$trainingData)) {
    column_names <- names(model$bestTune)
    column_values <- model$bestTune
    filtered <- model$pred %>%
      dplyr::filter_(paste(column_names, "==", shQuote(column_values), collapse = "&"))
    return(filtered)
  }
  model$pred
}

add_observed <- function(agg_data, model) {
  outcome <- attr(model$terms, "variables")[[2]]

  data <- save_filter(model)
  observed <- data$obs[orderer(data)]
  agg_data <- agg_data %>%
    tibble::add_column(!!(outcome) := observed)
}


