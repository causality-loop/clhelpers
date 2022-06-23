#' @importFrom magrittr '%<>%'
#' @title calculate_portfolio_return
#' @description Adds columns for cash before calculating the portfolio return.
#' @param asset_rets_xts xts, an xts of unconditional asset returns
#' @param asset_units_xts xts, an xts of units to be joined to the asset returns to form a portfolio return
#' @param verbose boolean, passed to PerformanceAnalytics::Return.portfolio(), Default: FALSE
#' @return See documentation for PerformanceAnalytics::Return.portfolio().
#' @details This is just a wrapper for PerformanceAnalytics::Return.portfolio().  See documentation for PerformanceAnalytics::Return.portfolio().
#' @export 
#' @importFrom PerformanceAnalytics Return.portfolio
calculate_portfolio_return <- function(asset_rets_xts, 
                                       asset_units_xts, 
                                       verbose = FALSE)
{
  asset_rets_xts %<>% cbind(CASH = 0)
  asset_units_xts %<>% cbind(CASH = 1-rowSums(.))
  PerformanceAnalytics::Return.portfolio(asset_rets_xts, asset_units_xts,
    verbose = verbose)
}

