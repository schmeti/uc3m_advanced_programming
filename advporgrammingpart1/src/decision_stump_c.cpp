#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// Normal

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


//double gini_impurity_c(List left, List right){}


// [[Rcpp::export]]
void best_split_c(DataFrame X,CharacterVector y) {
// init variables
  double best_gini = 1.0;
  double best_feature = -1;
  int n_features = X.size();
  
  for (int n = 0; n < n_features; ++n){ // iterate over columns
    Rcout << "Processing feature: " << n << std::endl;

    CharacterVector column = X[n]; // column as numeric vector
    int n_rows = column.size();
    
    CharacterVector unique_values; // vector for unique values
    unique_values = Rcpp::unique(column);
    int i_unique_values = unique_values.size();

    
    for(int i = 0; i<i_unique_values; ++i){ // iterate over unique values in column
      CharacterVector left_array;
      CharacterVector right_array; 
      for(int r = 0; r < n_rows; ++r){ // iterate over row elements in column and check wether is equal to unique value or not
        
        Rcout << "x: " << column[r] << std::endl;
        Rcout << "compar to unique: " << unique_values[i] << std::endl;
        Rcout << "y: " << y[r] << std::endl;
        
        if(column[r]==unique_values[i]){
          left_array.push_back(y[r]);
        } else {
          right_array.push_back(y[r]);
        }
      }
      Rcout << "left: " << left_array << std::endl;
      Rcout << "right: " << right_array << std::endl;
    }
  }
}



// [[Rcpp::export]]
void fit_decision_stump_c(DataFrame X,CharacterVector y){
  best_split_c(X, y);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
#Rcpp::sourceCpp("decision_stump_c.cpp")

load("../data/play_tennis.rda")
data <- play_tennis

n = best_split_c(X =data[, c("Outlook", "Temperature", "Humidity", "Wind")],
                     y =data[, c("PlayTennis")])
print(n)
*/
