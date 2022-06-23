#' @title show_special_day
#' @description Shows if today is the first/last trading day of the week/month.
#' @return A string indicating any of the three: EOM (end of month), SOM (start of month), EOW (end of week).
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  clhelpers::show_special_day()
#'  }
#' }
#' @export 
show_special_day <- function()
{
  if (!file.exists('data/dates.rds')) make_dates()
  dates <- readRDS('data/dates.rds')
  out <- sapply(dates, function(x) Sys.Date() %in% x)
  out[1:3] %>% .[.] %>% names
}

