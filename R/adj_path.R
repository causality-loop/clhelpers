#' @title adj_path
#' @description Adjusts a path by removing, if present, an ending forward slash.
#' @param path character, the path to be adjusted
#' @return The path without the ending forward slash.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  adj_path('~/git/some_model/')
#'  }
#' }
#' @export 
adj_path <- function(path) paste(unlist(strsplit(path, '/')),collapse='/')

