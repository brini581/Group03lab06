#' Greedy heuristic function that solves Knapsack problem
#' @param x A Data Frame with two variables(x and w)
#' @param W The Total weight
#' @return The Value and the elements
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#' @export


greedy_knapsack <- function (x,W){

  if(!is.data.frame(x) || !is.numeric(W)|| as.numeric(W)<0){
    stop("The function is expecting Data Frame and Numeric Value only !!!!")
  }

  if(!("v" %in% colnames(x)) || !("w" %in% colnames(x)) || !length(colnames(x))){
    stop("The data frame is expected to have two columns only (v and w)!!!!")
  }

  does_df_has_negative <- apply(x, 2, function(column) any(column < 0))

  if(length(which(does_df_has_negative))!= 0){
    stop("The data frame is expected to contain only positive values!!!!")
  }

  values_vector <- as.vector(unlist(x["v"]))
  weight_vector <- as.vector(unlist(x["w"]))
  cost_vector <-c()
  result <- c()
  result_elements <-c()
  j <- 1


  for(i in 1:length(values_vector)){
    cost_vector <- append(cost_vector,(values_vector[i]/weight_vector[i]))

  }
  df_vectors <-data.frame(id=c(1:length(values_vector)),values_vector,weight_vector,cost_vector)
  df_data <- df_vectors[order(df_vectors$cost_vector, decreasing = TRUE),]
  values_vector <- as.vector(unlist(df_data["values_vector"]))
  weight_vector <- as.vector(unlist(df_data["weight_vector"]))
  id_vector <- as.vector(unlist(df_data["id"]))


  while(j <= length(values_vector)){

    if(weight_vector[j] <= W){

      W <- W - weight_vector[j]
      result <- append(result, values_vector[j])

      result_elements <-append(result_elements,id_vector[j] )

    }else{
      break
    }
    j <- j+1


  }

  result_list <- list(value=round(sum(result)),elements=result_elements)
  return(result_list)
}


