#' Compares test MSE between different sets of predictors
#'
#' @input dat A dataset
#' @input response_var An unquoted variable name
#' @input pred_var_1 An unquoted variable name
#' @input pred_var_2 An unquoted variable name
#'
#' @return A data frame
#'
#' @import dplyr

compare_models <- function(dat, response_var, pred_var_1, pred_var_2) {

  rand <- sample(1:nrow(dat),
                 round(nrow(dat)/5))

  train_dat <- dat[-rand,]
  test_dat <- dat[rand,]

  beta_options <- make_betas(train_dat, {{response_var}}, {{pred_var_1}}, {{pred_var_2}})

  for(i in 1:3) {

    pred_res <- predict_from_coefs(test_dat,
                       {{response_var}},
                       beta_options[i,])

    mse <- get_mse(pred_res)

  }

  results <- data.frame(
    MSE = mse,
    model = c("First Var Only", "Second Var Only", "Both Vars")
  )

  return(results)


}


#' Computes predicted values given coefficients
#'
#' This function takes a data frame of coefficients in the form outputted by
#' the \code{make_betas} function.
#'
#' It calculates the predicted values of a response variable from these coefficients.
#'
#' @param dat A data frame
#' @param response_var The name of a response variable in the data frame (unquoted)
#' @param coefs A data frame of coefficient estimates
#'
#' @return A data frame of true and predicted values
#'
#' @import dplyr
#'
#' @export
predict_from_coefs <- function(dat, response_var, coefs){

  y <- dat %>%
    pull({{response}}) %>%
    as.matrix()

  X <- dat %>%
    select(-{{response}}) %>%
    mutate(
      Intercept = 1
    ) %>%
    as.matrix()

  betas <- as.matrix(coefs)

  preds <- t(betas %*% t(X))

  results <- data.frame(
    Observed = y,
    Predicted = preds
  )

  return(results)
}


#' Computes MSE from predicted values
#'
#' This function takes a data frame of predictions in the form outputted by
#' the \code{predict_from_coefs} function.
#'
#' @param results A data frame of predictions and truths
#' @param truth The name of the observed values column
#' @param estimate The name of the predicted values column
#'
#' @return The MSE
#'
#' @import dplyr
#'
#' @export
get_MSE <- function(results, truth, estimate){

  obs <- results %>% select({{truth}})
  est <- results %>% select({{estimate}})

  mse <- mean((obs - est)^2)

  return(mse)
}

