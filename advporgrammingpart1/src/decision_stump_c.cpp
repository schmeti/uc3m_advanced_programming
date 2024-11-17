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


// Utils




// Normal

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


//double gini_impurity_c(List left, List right){}


// [[Rcpp::export]]
List best_split_c(DataFrame X,DataFrame y) {
  double best_gini = 1.0;
  double best_feature = -1;
  int n_features = X.size();
  
  List values;
  
  for (int n = 0; n < n_features; ++n){
    Rcout << n;
    values = X[n];
    
    
  }
    
  return values;
}

// [[Rcpp::export]]
void fit_decision_stump_c(DataFrame X,DataFrame y){
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
