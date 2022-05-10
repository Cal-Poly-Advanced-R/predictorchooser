
#' Makes a basic scatterplot comparing a quantitative predictor to a response variable.
#'
#' @param dat A dataset
#' @param response_var An unquoted variable name
#' @param pred_var An unquoted variable name
#'
#' @export
viz_quant_pred <- function(dat, response_var, pred_var) {

  dat %>%
    ggplot(aes(x = {{pred_var}},
               y = {{response_var}})) %>%
    geom_point()

}


#' Makes a basic barplot comparing a categorical predictor to a response variable.
#'
#' @param dat A dataset
#' @param response_var An unquoted variable name
#' @param pred_var An unquoted variable name
#'
#' @import ggplot2
#' @export
viz_cat_pred <- function(dat, response_var, pred_var) {

  dat %>%
    ggplot(aes(x = {{pred_var}},
               y = {{response_var}})) %>%
    geom_bar()

}
