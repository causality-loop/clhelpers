#' @title calculate_returns
#' @description Wrapper for TTR:ROC() but with easy periodicity changing.
#' @param closes_xts xts, an xts of closing prices
#' @param ret_periodicity character, the return periodicity which is passed to xts::endpoints(), Default: 'day'
#' @return An xts of asset returns.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  quantmod::getSymbols('SPY')
#'  SPY[,4] %>%
#'    clhelpers::calculate_returns('week')
#'  }
#' }
#' @export 
#' @importFrom xts endpoints
#' @importFrom TTR ROC
calculate_returns <- function(closes_xts, ret_periodicity = 'day') 
{
  closes_xts[xts::endpoints(closes_xts, ret_periodicity)] %>%
    TTR::ROC(type = 'discrete', na.pad = FALSE)
}

