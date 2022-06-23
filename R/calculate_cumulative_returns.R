#' @title calculate_cumulative_returns
#' @description Calculates the cumulative return for each column of an xts.
#' @param rets_xts xts, an xts of asset returns
#' @return A matrix displaying the cumulative return for each column of returns.
#' @details A minimal `PerformanceAnalytics::Return.cumulative()`.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  quantmod::getSymbols('SPY')
#'  SPY[,4] %>%
#'    {. / stats::lag(.) - 1} %>%
#'    stats::na.omit() %>%
#'    clhelpers::calculate_cumulative_returns()
#'  }
#' }
#' @export 
calculate_cumulative_returns <- function(rets_xts)
{
  apply(rets_xts, 2, function(x) prod(as.numeric(x)+1)-1) %>% t %>% 
    `rownames<-`('Cumulative Return')
}

