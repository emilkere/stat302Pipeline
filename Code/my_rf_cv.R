#' Random Forest Cross Validation
#'
#' This function performs Cross Validation for Random Forest with 100 trees.
#'
#' @param k numeric input, representing the number of folds used, defaults
#'   to \code{5}
#'
#' @keywords prediction
#'
#' @return A numeric with with the cross-validation error.
#'
#' @examples
#' ## performs cross-validation with 5 folds
#' my_rf_cv()
#'
#' ## performs cross-validation with n folds
#' k_cv <- 3
#' my_rf_cv(k = k_cv)
#' @export
my_rf_cv <- function(k = 5) {
  if (k <= 1) {
    stop("Number of folds k,  for cross validation needs to be bigger than 1.")
  }

  # prepare penguin data
  # select body_mass_g, bill_length_mm, bill_depth_mm, and flipper_length_mm
  # remove na since randomForest does not handle them
  peng_no_na <- my_penguins %>%
    select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm) %>%
    drop_na()

  # data frame with only the covariates
  peng_cov <- peng_no_na %>% select(-body_mass_g)
  # only body mass
  peng_body_mass_g <- peng_no_na$body_mass_g

  len <- nrow(peng_no_na)

  # generate random folds
  # within your function, define a variable fold
  fold <- sample(rep(1:k, length = len))

  # vector for misclassification rate at each step
  mse <- rep(0, k)


  for (j in 1:k) {
    tr_id <- which(fold != j)
    tst_id <- which(fold == j)

    train <- peng_no_na[tr_id, ]
    test_cov <- peng_cov[tst_id, ]
    test_val <- peng_body_mass_g[tst_id]

    # use randomForest() from the randomForest package
    # train a random forest model with 100 trees
    # to predict body_mass_g using covariates bill_length_mm,
    # bill_depth_mm, and flipper_length_mm.
    mrf_m <- randomForest::randomForest((body_mass_g ~ bill_length_mm +
      bill_depth_mm + flipper_length_mm), data = train, ntree = 100)
    # use model to predict
    pred <- predict(mrf_m, test_cov)

    npreds <- length(tst_id)
    # calculdating MSE
    mse[j] <- sum((pred - test_val)^2) / npreds
  }
  mean_mse <- mean(mse)

  # Return the average MSE across all k folds
  return(mean_mse)
}
