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

  for(i in 1:number) seeds[[i]]<- sample.int(n=1000, 4)
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
#' @param seed starting seed for list of seeds
#' @param number number of folds or resamples
#'
#' @return tibble or list of tibbles with out of fold predictions
#' for each model.
#'
#' @examples
#' trainControl(method = 'cv', number = 10, seeds = caret_seed(1234, 10))
#'
#' @importFrom magrittr %>%
#' @export
oof_grab <- function(models, train_data, type = 'raw') {
  if (inherits(models, "train")) models <- list(models)

  agg_data <- purrr::map_dfc(models, grabber, type)

  template_model <- models[[1]]
  outcome <- attr(template_model$terms, "variables")[[2]]
  outcome_data <- template_model$pred$obs[orderer(template_model)]
  agg_data <- agg_data %>%
    tibble::add_column(!!(outcome) := outcome_data)
  agg_data
}

grabber <- function(model, type) {

  if (is.null(model$pred)) {
    stop("Out of fold predictions were not saved in the caret model. ",
         "Re-run with savePredictions = 'final' in trainControl.")
  }
  grabbers <- list(raw = pred_grabber, prob = prob_grabber)
  grab <- grabbers[[type]]

  if (is.null(grab)) stop("Not a valid type. Use raw or prob.")

  grab(model)
}

# need to add warning if probabilities weren't saved
prob_grabber <- function(model) {
  columns <- as.character(unique(model$trainingData$.outcome))
  if (all(!columns %in% names(model$pred))) {
    stop("Probabilities were not saved, or are not available in the caret model. ",
         "Re-run with classProbs = TRUE in trainControl.")
  }
  tibble::as_tibble(model$pred[orderer(model), columns])
}

pred_grabber <- function(model) {
  if (nrow(model$pred) != nrow(model$trainingData)) {
    stop("All resamples were saved, so oof_grab doesn't know which to pick in",
         "the caret model.", "Re-run with savePredictions = 'final' in trainControl.")
  }
  tibble::as_tibble(model$pred$pred[orderer(model)])
}

orderer <- function(model) {
  order(model$pred$rowIndex)
}



