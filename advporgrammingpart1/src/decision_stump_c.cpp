#include <Rcpp.h>
#include <cmath>
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


// [[Rcpp::export]]
double value_count(CharacterVector vec, const std::string& target){
  int len_vec = vec.size();
  int count = 0;
  for(int n=0;n<len_vec;++n){
    if (std::string(vec[n]) == target){
      ++count;
    }
  }
  return count;
}


// [[Rcpp::export]]
double gini_impurity_c(CharacterVector left, CharacterVector right){
  double len_left = left.size();
  double len_right = right.size();
  double len_total = len_left + len_right;
  
  double gin_right = 1 - (std::pow(value_count(right, "Yes") / len_right, 2) +
                          std::pow(value_count(right, "No") / len_right, 2));
  double gin_left = 1 - (std::pow(value_count(left, "Yes") / len_left, 2) +
                         std::pow(value_count(left, "No") / len_left, 2));
  
  //Rcout << "gin_right: " << gin_right << std::endl;
  //Rcout << "gin_left: " << gin_left << std::endl;
  
  
  
  
  double weight_gini = (len_left/len_total)*gin_left+(len_right/len_total)*gin_right;
  
  return weight_gini;
}


// [[Rcpp::export]]
List best_split_c(DataFrame X, List features, CharacterVector y) {
// init variables
  int n_features = X.size();
  
  double best_gini = 100000;
  std::string best_feature;
  std::string best_value;
  
  
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
        if(column[r]==unique_values[i]){
          left_array.push_back(y[r]);
        } else {
          right_array.push_back(y[r]);
        }
      }
      
      double weight_gini = gini_impurity_c(left_array, right_array);
      // Print gini
      //Rcout << "right_array: " << right_array << std::endl;
      //Rcout << "left_array: " << left_array << std::endl;
      //Rcout << "weight_gini: " << weight_gini << std::endl;
      
      if (weight_gini < best_gini){
        best_gini = weight_gini;
        best_feature = Rcpp::as<std::string>(features[n]); // Explicit conversion to std::string
        best_value = Rcpp::as<std::string>(unique_values[i]); 
      }
    }
  }
  return List::create(Named("best_gini") = best_gini,
                      Named("best_feature") = best_feature,
                      Named("best_value") = best_value);
}



// [[Rcpp::export]]
void fit_decision_stump_c(DataFrame X, List features, CharacterVector y){
  best_split_c(X, features, y);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
#Rcpp::sourceCpp("decision_stump_c.cpp")

load("../data/play_tennis.rda")
data <- play_tennis

best_stump = best_split_c(X =data[, c("Outlook", "Temperature", "Humidity", "Wind")],
                 features = list("Outlook", "Temperature", "Humidity", "Wind"),
                 y = data[["PlayTennis"]])

print(best_stump)
*/


