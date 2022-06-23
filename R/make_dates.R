.datatable.aware = TRUE 
if (getRversion() >= '2.15.1') 
  utils::globalVariables(c('.', 'DoW', 'Date'), utils::packageName()) 

#' @title make_dates
#' @description Produces a list of dates on which the market is open; indicates start/end-of-month days, as well as end-of-week.
#' @details Creates a directory called `data` which contains the dates RDS file.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  clhelpers::make_dates()
#'  }
#' }
#' @export 
#' @importFrom data.table ':='
#' @importFrom xml2 read_html
#' @importFrom lubridate year mdy wday
#' @importFrom rvest html_nodes html_table
#' @importFrom data.table data.table
#' @importFrom xts endpoints
make_dates <- function()
{

  if (!dir.exists('data')) dir.create('data', mode = '0744')

  year <- lubridate::year(Sys.Date())
  web_dates <- xml2::read_html(
    'https://www.nyse.com/markets/hours-calendars') %>%
    rvest::html_nodes('table') %>% .[[1]] %>% rvest::html_table()

  early_close_dates <- data.table::data.table(web_dates) %>%
    .[, get(as.character(year))] %>%
    iconv('latin1', 'ASCII', sub = '') %>% # remove non-ASCII chars
    grep(pattern = '\\*', value = TRUE) %>% # keep dates with wildcards
    gsub(pattern = '\\s*\\([^\\)]+\\)', replacement = '') %>% # rm any in ()
    gsub(pattern='\\*', replacement='') %>% # remove wildcards, but keep dates
    .[. != ''] %>% # remove blanks
    sapply(function(x) {
      paste0(x, ', ', year) %>% lubridate::mdy() %>% as.character() }) %>%
    as.Date() %>% unname()

  holidays <- data.table::data.table(web_dates) %>%
    .[, get(as.character(year))] %>%
    iconv('latin1', 'ASCII', sub = '') %>% # remove non-ASCII chars
    .[. != ''] %>% # remove blanks (caused by above fun)
    grep(pattern = '\\*', value = TRUE, invert = TRUE) %>% #rm chars w wildcard
    gsub(pattern = '\\s*\\([^\\)]+\\)', replacement = '') %>% # rm any in ()
    sapply(function(x) {
      paste0(x, ', ', year) %>% lubridate::mdy() %>% as.character() })

  market_open_dates <- data.table::data.table(
    seq.Date(as.Date(paste0(year, '-01-01')), as.Date(paste0(year, '-12-31')),
             1)) %>%
    `names<-`('Date') %>%
    .[, DoW := lubridate::wday(Date, label = FALSE, week_start = 1)] %>%
    .[DoW %ni% c(6,7)] %>% .[Date %ni% as.Date(holidays)] %>% .[, Date]

  eom_dates <- xts::endpoints(market_open_dates) %>% {market_open_dates[.]}
  som_dates <- xts::endpoints(market_open_dates) %>% {.[-length(.)]} %>%
    {. + 1} %>% {market_open_dates[.]}
  eow_dates <- xts::endpoints(market_open_dates, 'week') %>%
    {market_open_dates[.]}
  dates <- list(EOM = eom_dates, SOM = som_dates, EOW = eow_dates, 
    market_open_dates=market_open_dates, early_close_dates=early_close_dates)
  saveRDS(dates, file = paste0('data/dates.rds'))

}

