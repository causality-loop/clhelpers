#' @title make_closes_xts
#' @description You probably don't need to use this.
#' @param asset_symbols character, a string of asset_symbols
#' @param n integer, passed to utils::tail(), Default: 430
#' @return An xts of closes.
#' @export 
#' @importFrom utils tail
#' @importFrom xts merge.xts
#' @importFrom zoo na.approx
make_closes_xts <- function(asset_symbols, n = 430)
{
  lapply(asset_symbols, function(x) {
    readRDS(file.path('prices', x)) %>% utils::tail(n) %>% .[,4]
  }) %>%
    do.call(what = xts::merge.xts) %>%
    zoo::na.approx() %>%
    `names<-`(asset_symbols)
}

