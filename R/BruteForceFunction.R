#' @title Computes the knapsack problem
#' @description An approach to solve the knapsack problem using brute force
#' @param x A dataframe with 2 columns, w weights and v values of each item
#' @param W Weight of the knapsack
#'
#' @return Returns the maximum value whose weight is lower than W and the elements
#' @export
#'

brute_force_knapsack <- function(x, W) {

  stopifnot(is.data.frame(x))
  stopifnot(W > 0)
  stopifnot(length(x) == 2)
  stopifnot(names(x)[1] == "w" && names(x)[2] == "v")

  len <- length(x$w)
  weight <- vector(length = length(c(1 : (( 2 ^ len ) - 1))))
  value <- vector(length = length(c(1 : (( 2 ^ len ) - 1))))
  df_new <- data.frame(weight, value)
  elements <- list()
  for(i in c(1 : ( 2 ^ len ) - 1)) {
    wei <- sum(x$w[which(intToBits(i)[1:len] == 1)])
    if(wei <= W) {
      df_new$weight[i] <- wei
      df_new$value[i] <- sum(x$v[which(intToBits(i)[1:len] == 1)])
      elements[i] <- list(which(intToBits(i)[1:len] == 1))
    }
  }
  result <- list(value = round(max(df_new$value)), elements = unlist(elements[which.max(df_new$value)]))
  return(result)
}
