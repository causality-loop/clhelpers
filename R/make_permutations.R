#' @title make_permutations
#' @description Creates a matrix which shows all the permutations of a series.
#' @param x, any kind of vector
#' @return A matrix of all permutations.
#' @details Taken from Stack Overflow.
#' @author "Adrian"
#' @references
#' \url{https://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r?answertab=scoredesc#tab-top}
#' @export 
make_permutations <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) res <- rbind(res, cbind(x[i], Recall(x[-i])))
    return(res)
  }
}

