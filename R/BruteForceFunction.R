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
  df_colnames <- names(x)
  stopifnot(length(x) == 2)
  stopifnot(df_colnames[1] == "w" && df_colnames[2] == "v")

  len <- length(x$w)
  poss <- c(0 : (( 2 ^ len ) - 1))
  bits <- sapply(poss, intToBits)
  weight <- vector(length = length(poss))
  value <- vector(length = length(poss))
  df_new <- data.frame(weight, value)
  elements <- list()
  for(i in c(1 : length(poss))){
    val <- vector(length = length(poss))
    wei <- vector(length = length(poss))
    ele <- vector()
    for(j in c(1 : 32)){
      if(bits[j, i] == 1){
        wei[i] <- wei[i] + x$w[j]
        val[i] <- val[i] + x$v[j]
        ele <- c(ele, j)
      }else{
        wei[i] <- wei[i]
        val[i] <- val[i]
      }
    }
    if(isTRUE(wei[i] <= W)){
      df_new$weight[i] <- wei[i]
      df_new$value[i] <- val[i]
      elements[i] <- c(list(ele))
    } else {
      df_new$weight[i] <- 0
      df_new$value[i] <- 0
      elements[i] <-  c(0)
    }
  }
  index <- which.max(df_new$value)
  elem <- unlist(elements[index])
  max_va <- round(max(df_new$value))
  result <- list(value = max_va, elements = elem)
  return(result)
}
