#' @title calculate_sd
#' @description A minimal PerformanceAnalytics::StdDev().
#' @param rets_xts xts, an xts of returns
#' @return A matrix of standard deviations.
#' @export 
#' @importFrom stats sd
calculate_sd <- function(rets_xts)
{
  apply(rets_xts, 2, stats::sd) %>% t %>% `rownames<-`('StdDev')
}

