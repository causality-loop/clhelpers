.datatable.aware = TRUE 
if (getRversion() >= '2.15.1') 
  utils::globalVariables(c('.', 'Symbol', 'Date added', 'Added', 'Removed'), utils::packageName()) 
 
#' @title scrape_wiki_dow_components
#' @description scrapes dates for when particular Dow components were added/removed in a timeframe
#' @param from_date character, representing a date in YYYY-MM-DD format, it indicates the date which fills in missing date values
#' @return a data.table with two columns: 1) Symbol and 2) Date_Range (a range of days)
#' @references [DJIA Wiki](https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average)
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  clhelpers::scrape_wiki_dow_components()
#'  }
#' }
#' @export 
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom data.table data.table
scrape_wiki_dow_components <- function(from_date = '2007-01-01')
{

  xml2::read_html(
    'https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average') %>%
    rvest::html_nodes('table') %>%
    .[[2]] %>%
    rvest::html_table() %>%
    data.table::data.table() %>%
    .[, .(Symbol, Added = as.Date(`Date added`), Removed = Sys.Date())] %>%
    rbind.data.frame(
      c('MO', from_date, '2008-02-15'),
      c('HON', from_date, '2008-02-15'),
      c('AIG', from_date, '2008-09-19'),
      c('C', from_date, '2009-06-05'),
      c('GM', from_date, '2009-06-05'),
      c('KHC', '2008-09-22', '2012-09-21'),
      c('AA', from_date, '2013-09-19'),
      c('BAC', '2008-02-19', '2013-09-19'),
      c('HPQ', from_date, '2013-09-19'),
      c('T', from_date, '2015-03-18'),
      c('GE', from_date, '2018-06-25'),
      c('DD', from_date, '2019-04-01'),
      c('XOM', from_date, '2020-08-28'),
      c('PFE', from_date, '2020-08-28'),
      c('RTX', from_date, '2020-08-28')
    ) %>%
    # move all dates in `Added` back 1 year to reveal the return series for the
    # cor() fun, so that they can be traded right after being added to the index
    .[, .(Symbol, Date_Range = paste0(Added-366, '/', Removed))] %>%
    .[order(Symbol)]

}

