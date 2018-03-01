#' Set seed for \code{caret}'s \code{trainControl}
#'
#' Given a starting seed and number of folds (or resamples),
#' this function creates a vector of seeds needed for the `caret`
#' trainControl object.
#'
#' @param seed starting seed for list of seeds
#' @param k_fold number of folds or resamples
#'
#' @return List of seeds used by \code{trainControl}.
#'
#' @examples
#' trainControl(method = 'cv', number = 10, seeds = caret_seed(1234, 10))
#'
#' @export
caret_seed <- function(seed = 1, k_fold) {
  set.seed(seed)
  seeds <- vector(mode = "list", length = k_fold + 1)

  for(i in 1:k_fold) seeds[[i]]<- sample.int(n=1000, 4)
  seeds[[k_fold + 1]] <- sample.int(1000, 1)
  seeds
}

