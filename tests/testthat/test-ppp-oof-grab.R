context("oof")

library(caret)

test_that("oof_grab works with one model or a list", {
  m1 <- readRDS(here::here("models_one", "model1.RDS"))
  m2 <- readRDS(here::here("models_one", "model2.RDS"))

  m1_oof <- oof_grab(m1)
  listed_oof <- oof_grab(list(m1, m2))

  expect_identical(m1_oof, listed_oof[,-2])
})

test_that("oof_grab returns identical outcomes as training data", {
  m1 <- readRDS(here::here("models_one", "model1.RDS"))
  m1_oof <- oof_grab(m1)

  expect_identical(m1_oof$Species, iris$Species)
})

test_that("oof_grab handles probabilities", {
  m1 <- readRDS(here::here("models_one", "model1.RDS"))
  m1_oof <- oof_grab(m1, type = 'prob')

  expect_equal(length(m1_oof), length(unique(iris$Species)) + 1)

  m2 <- readRDS(here::here("models_one", "model2.RDS"))
  listed_oof <- oof_grab(list(m1, m2), type = 'prob')

  expect_equal(length(listed_oof), length(unique(iris$Species)) * 2 + 1)
})

test_that("oof_grab throws an error for invalid type", {
  m1 <- readRDS(here::here("models_one", "model1.RDS"))
  expect_error(m1_oof(m1, type = "wrong_type"))
})

test_that("oof_grab throws an error if classProbs = FALSE in trainControl", {
  control = trainControl(method = 'cv', number = '5', classProbs = FALSE, savePredictions = 'final')
  model <- train(Species ~ ., data = iris, method = 'rf', trControl = control)
  expect_error(oof_grab(model, type = 'prob'), "Probabilities were not saved, or are not available in the caret model*")
})

test_that("oof_grab throws an error if savePredictions = 'none'", {
  model <- train(Species ~ ., data = iris, method = 'rf')
  expect_error(oof_grab(model), "Out of fold predictions were not saved in the caret model*")
  expect_error(oof_grab(model, type = 'prob'), "Out of fold predictions were not saved in the caret model*")
})

test_that("oof_grab grabs best predictions if savePredictions = 'all'", {
  control1 = trainControl(method = 'cv', number = 5, savePredictions = 'all')
  set.seed(123)
  m1 <- train(Species ~ ., data = iris, method = 'rf', trControl = control1)

  control2 = trainControl(method = 'cv', number = 5, savePredictions = 'final')
  set.seed(123)
  m2 <- train(Species ~ ., data = iris, method = 'rf', trControl = control2)

  expect_identical(oof_grab(m1), oof_grab(m2))
})

test_that("oof_grab grabs the best predictions if there are multiple tuning parameters", {
  control1 = trainControl(method = 'cv', number = 5, savePredictions = 'all')
  set.seed(456)
  m1 <- train(Species ~ ., data = iris, method = 'glmnet', trControl = control1)

  control2 = trainControl(method = 'cv', number = 5, savePredictions = 'final')
  set.seed(456)
  m2 <- train(Species ~ ., data = iris, method = 'glmnet', trControl = control2)

  expect_identical(oof_grab(m1), oof_grab(m2))
})


