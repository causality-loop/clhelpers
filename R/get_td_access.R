#' @title get_td_access
#' @description Quick access to TD Ameritrade's API.
#' @param full_path_to_td_credentials character, the full path to the directory containing the consumer key and refresh token for the *rameritrade* package
#' @details See the below under *References* for directions on creating the relevant files.  Once they are created, put them in a directory and point this function to that directory for API access.
#' @references
#' \url{https://github.com/exploringfinance/rameritrade/}
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  get_td_access('~/td')
#'  }
#' }
#' @export 
#' @importFrom rameritrade td_auth_accessToken
get_td_access <- function(full_path_to_td_credentials)
{
  path <- adj_path(full_path_to_td_credentials)
  consumerKey <- readRDS(file.path(path, 'consumerKey.rds'))
  refreshToken <- readRDS(file.path(path, 'refreshToken.rds'))
  accessToken <- rameritrade::td_auth_accessToken(consumerKey = consumerKey,
                                                  refreshToken = refreshToken)
}

