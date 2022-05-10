#' Calculates the coefficients of a multiple regression for different predictor combos.
#'
#' @param dat A dataset
#' @param response_var An unquoted variable name
#' @param pred_var_1 An unquoted variable name
#' @param pred_var_2 An unquoted variable name
#'
#' @return A data frame
#'
#' @import dplyr
#' @export

make_betas <- function(dat, response_var, pred_var_1, pred_var_2) {

  y <- dat %>%
    select(response_var) %>%
    as.matrix()

  X_1 <- dat %>%
    select({{pred_var_1}}) %>%
    as.matrix()

  X_2 <- dat %>%
    select({{pred_var_2}}) %>%
    as.matrix()

  X_12 <- dat %>%
    select({{pred_var_1}}, {{pred_var_2}}) %>%
    as.matrix()

  Xs <- list(X_1, X_2, X_12)

  purrr::map_dfr(Xs, ~compute_betas_once(y, .x))

}


#' Calculates the coefficients of a multiple regression using matrix decomposition
#'
#' @param y A n by 1 matrix of response variable values
#' @param X A n by p matrix of predictor values
#'
#' @return A data frame of coefficient estimates
#'
#' @export

compute_betas_once <- function(y, X) {

  betas <- solve(t(X) * X) * t(X) * y

  results <- as.data.frame(betas)

  names(results)[1] <- "Intercept"

}
