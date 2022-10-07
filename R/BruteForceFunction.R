#' @title Computes the knapsack problem
#' @description An approach to solve the knapsack problem using brute force
#' @param x A dataframe with 2 columns, w weights and v values of each item
#' @param W Weight of the knapsack
#' @param parr Logical to determine if it is to be executed in parallel or not
#'
#' @return Returns the maximum value whose weight is lower than W and the elements
#' @import parallel
#' @export
#'

brute_force_knapsack <- function(x, W, parr = FALSE) {

  stopifnot(is.data.frame(x))
  stopifnot(W > 0)
  stopifnot(length(x) == 2)
  stopifnot(names(x)[1] == "w" && names(x)[2] == "v")

  len <- length(x$w)
  weight <- vector(length = length(c(1 : (( 2 ^ len ) - 1))))
  value <- vector(length = length(c(1 : (( 2 ^ len ) - 1))))
  df_new <- data.frame(weight, value)
  elements <- list()

  if(parr == FALSE) {
    for(i in c(1 : ( 2 ^ len ) - 1)) {
      wei <- sum(x$w[which(intToBits(i)[1 : len] == 1)])
      if(wei <= W) {
        df_new$weight[i] <- wei
        df_new$value[i] <- sum(x$v[which(intToBits(i)[1 : len] == 1)])
        elements[i] <- list(which(intToBits(i)[1 : len] == 1))
      }
    }
    result <- list(value = round(max(df_new$value)),
                   elements = unlist(elements[which.max(df_new$value)]))
    return(result)
  }
  else {
    cl1 <- makeCluster(detectCores())
    clusterExport(cl1, varlist =c("x","len","df_new","W","weight","value"),
                  envir = environment())
    df_new <- parLapply(cl1, X = c(1 : ( 2 ^ len )), function(i) {
              weight <- sum(x$w[which(intToBits(i)[1 : len] == 1)])
              if(weight <= W)
                value <- sum(x$v[which(intToBits(i)[1 : len] == 1)])
              else
                value <- 0
              })
    stopCluster(cl1)
    max_value <- max(unlist(df_new))
    index <- which.max(unlist(df_new))
    result <- list(value = round(max_value),
                   elements = which(intToBits(index)[1 : len] == 1))
    return(result)
  }
}
