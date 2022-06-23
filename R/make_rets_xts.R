#' @title make_rets_xts
#' @description Makes an xts of returns from a string of symbols.
#' @param asset_symbols character, a string of asset_symbols
#' @param n integer, passed to utils::tail(), Default: 430
#' @return An xts of asset returns.
#' @export 
make_rets_xts <- function(asset_symbols, n = 430)
{
  make_closes_xts(asset_symbols, n) %>% calculate_returns
}

