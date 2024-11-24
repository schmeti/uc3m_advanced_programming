# You must program this function, first in R, then in C++
gini_impurity_R <- function(left,right){
  len_right= length(right) #length of the right 
  len_left = length(left) #length of the right
  n_rows = len_left+len_right #length of the total rows
  gin_left = 1 - ((sum(left=='Yes')/len_left)^2+(sum(left=='No')/len_left)^2) #gini calculation for the left
  gin_right = 1 - ((sum(right=='Yes')/len_right)^2+(sum(right=='No')/len_right)^2) #gini calculation for the right
  weight_gini = len_left/n_rows * gin_left + len_right/n_rows*gin_right #weighted
  return(weight_gini)
}

best_split_R_v2<- function(X, y) { #X is your predictors and y is the response variable
  best_gini <- 1.0 # the worst possible gini will be 1 so you leave it as a max possible value
  best_feature <- -1 #feature auxiliar only to be change for another one.
  n_features <- ncol(X) #number of columns to check
  
  for (feature in 1:n_features) { # runs through all the columns 
    values <- unique(X[, feature]) #gives all the unique values in the column i(feature) if you have categorical you will get the different levels
    
    # The for value in values divide the X in 2. The ones that are equal to the i value in values 
    # and the ones that are different from the value. 
    for (value in values) { 
      left_indices <- X[, feature] == value #vector value of the values that are equal to the value i
      right_indices <- X[, feature] != value
      
      left <- y[left_indices] #subset of element of y where left_indice is true
      right <- y[right_indices] #same but right_indice is true
      
      gini <- gini_impurity_R(left, right) #how much does the value (i) divide the dataset
      if (gini < best_gini) { #check if gini get improves (get smaller) and if this happen change the gini value and save the feature
        best_gini <- gini
        best_feature <- feature # Store the best feature index
        best_value <- value #which division of the values you use
  
  ind_left <- X[, best_feature] == best_value
  ind_right <- !ind_left
  left_t <- y[ind_left]
  right_t <- y[ind_right]
  get_majority_class <- function(target) {
    yes_count <- sum(target == "Yes")
    no_count <- sum(target == "No")
    if (yes_count > no_count) {
      return("Yes")
    } else if (yes_count == no_count) {
      return("Yes-No")
    } else {
      return("No")
    }
  }
  may_left <- get_majority_class(left_t)
  may_right <- get_majority_class(right_t)
      }
    }
  }
  
  output <- list( #give the save values for the split 
    best_feature = best_feature,
    best_value = best_value,
    best_gini = best_gini,
    left = left_t,
    right = right_t,
    mayority_left = may_left,
    mayority_right = may_right
  )
  
  output
}

# X is a numeric matrix with the input predictors
# y is a numeric vector with the response values (the two classes: 0 or 1)
# In order to obtain X and y from the play_tennis dataset, you'll need the 
#   num_matrix_from_df function.

fit_decision_stump_R_v2 <- function(X, y) {
  best_split_R_v2(X, y) # Call best_split and return the best feature index
}


