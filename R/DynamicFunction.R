#' @title Computes the knapsack problem
#' @description An approach to solve the knapsack problem using dynamic programming
#' @param x A Data Frame with two variables(x and w)
#' @param W The Total weight
#' @return The Value and the elements
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#' @export

knapsack_dynamic <- function(x, W){

  if(!is.data.frame(x) || !is.numeric(W)|| as.numeric(W)<0){
    stop("The function is expecting Data Frame and Positive Numeric Value only !!!!")
  }

  if(!("v" %in% colnames(x)) || !("w" %in% colnames(x)) || !(length(colnames(x))==2)){
    stop("The data frame is expected to have two columns only (v and w)!!!!")
  }

  does_df_has_negative <- apply(x, 2, function(column) any(column < 0))

  if(length(which(does_df_has_negative))!= 0){
    stop("The data frame is expected to contain only positive values!!!!")
  }

  values_vector <- as.vector(unlist(x["v"]))
  weight_vector <- as.vector(unlist(x["w"]))



  tabular_matrix <- matrix(0:0, nrow = nrow(x)+1, ncol =W+1)

  counter <- length(values_vector)+1


    for(i in 1:length(values_vector)){
      for(j in 1:W+1){
        if(weight_vector[i]>j){

          tabular_matrix[i+1, j] <- tabular_matrix[i+1-1, j]
        }
        else{

          tabular_matrix[i+1, j] <- max(tabular_matrix[i+1-1, j], tabular_matrix[i+1-1, j-weight_vector[i]] + values_vector[i])

        }
      }
    }

  result_table <-tabular_matrix[length(values_vector)+1,W+1]
  elements_vector <-c()
  w_track <-W+1
  while(counter >1 && result_table >0){

    if (result_table != tabular_matrix[counter-1,w_track]) {

      elements_vector <- append(elements_vector,counter - 1)
      result_table = result_table - values_vector[counter - 1];
      w_track = w_track - weight_vector[counter - 1];

    }
    counter <- counter-1
  }
  result_list <-list(value=as.vector(round(tabular_matrix[length(values_vector)+1,W+1])), elements=sort(elements_vector))

  return(result_list)

}

