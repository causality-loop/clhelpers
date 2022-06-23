clag <- function(x, k = 1)
{
  c(rep(NA, k), x)[1 : length(x)] %>% as.numeric
}

#' @importFrom magrittr '%T>%' 
#' @title lag_col
#' @description Given a matrix, xts, data frame, or data table, lag the values of a specified column.
#' @param x matrix, xts, data frame or data table
#' @param col_num integer, the column number to lag
#' @param k integer, lag length, Default: 1
#' @return Object of the same format with values in one column lagged.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  library(magrittr)
#'  quantmod::getSymbols('SPY;DIA')
#'  x <- lapply(c('SPY','DIA'), function(x) get(x)[,4]) %>% do.call(what=cbind)
#'  x[1:5] %>% lag_col(1, 3)
#'  }
#' }
#' @export 
lag_col <- function(x, col_num, k = 1)
{
  x %T>% { .[,col_num] <- clag(.[,col_num], k) }
}

