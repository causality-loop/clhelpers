#' @title get_all_my_prices
#' @description It's the place where you record all your symbols and manually update their prices.  Customize the source code to your needs.  
#' @return Apart from updating prices, it displays a string indicating which symbols were downloaded and are being stored.
#' @export 
#' @importFrom clhelpers scrape_wiki_dow_components
#' @importFrom updateprices append_asset_info update_prices ls_prices
get_all_my_prices <- function()
{

  bav_symbols <- 'SPY'
  gan_symbols <- c('SPY', 'DIA', 'QQQ')
  vim_symbols_test <- unique(clhelpers::scrape_wiki_dow_components()[,Symbol])
  can_symbols <- c('SPY', 'VWO', 'AGG')
  kda_symbols <- c('SPY', 'VGK', 'EWJ',  'EEM',  'VNQ',  'RWX',  'IEF', 'TLT', 'DBC', 'GLD', 'VWO', 'AGG')  

  asset_info <- c(bav_symbols, gan_symbols, vim_symbols_test, can_symbols) %>%
    unique %>%
    sort %>% 
    updateprices::append_asset_info('^VIX', '1990-01-01')

  updateprices::update_prices(asset_info, 'Dow', market_open_check = FALSE)
  updateprices::ls_prices()

}

