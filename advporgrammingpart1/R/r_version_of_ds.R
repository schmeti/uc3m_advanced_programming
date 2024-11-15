# You must program this function, first in R, then in C++
gini_impurity_R <- function(){
}

best_split_R <- function(X, y) {
  best_gini <- 1.0
  best_feature <- -1
  n_features <- ncol(X)
  
  for (feature in 1:n_features) {
    values <- unique(X[, feature])
    
    for (value in values) {
      left_indices <- X[, feature] == value
      right_indices <- X[, feature] != value
      
      left <- y[left_indices]
      right <- y[right_indices]
      
      gini <- gini_impurity_R(left, right)
      
      if (gini < best_gini) {
        best_gini <- gini
        best_feature <- feature # Store the best feature index
        best_value <- value
      }
    }
  }
  
  output <- list(
    best_feature = best_feature,
    best_value = best_value,
    best_gini = best_gini
  )
  
  output
}

# X is a numeric matrix with the input predictors
# y is a numeric vector with the response values (the two classes: 0 or 1)
# In order to obtain X and y from the play_tennis dataset, you'll need the 
#   num_matrix_from_df function.

fit_decision_stump_R <- function(X, y) {
  best_split_R(X, y) # Call best_split and return the best feature index
}

