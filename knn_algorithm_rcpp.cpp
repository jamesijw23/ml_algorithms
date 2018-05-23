#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

//-----------------------------------
// Name: repbyrow
// Input: 
// a) v1 --> Vector of values
// b) num_row --> rnumber of rows repeat
// Output: Matrix of vector repeated by row
// Purpose: Repeat a vector into num_row times by rows
// Improvements: 
// a) No for loop 
//-----------------------------------
// [[Rcpp::export]]
arma::mat repbyrow(arma::vec v1,int num_col){
  arma::mat matrix_v1(v1.size(),num_col);
  for(int i=0;i<num_col;i++){
    matrix_v1.col(i) = v1;
  }
  return arma::trans(matrix_v1);
}


// [[Rcpp::export]]
arma::colvec euclidean_dist_matrix(arma::mat mat1,arma::vec v1) {
  int num_rows = mat1.n_rows;
  arma::mat mat2 = repbyrow(v1,num_rows);
  arma::colvec sqrt_vec = sqrt(sum(pow(mat1 - mat2,2),1));
  return sqrt_vec;
}

// [[Rcpp::export]]
arma::colvec absolute_dist_matrix(arma::mat mat1,arma::vec v1) {
  int num_rows = mat1.n_rows;
  arma::mat mat2 = repbyrow(v1,num_rows);
  arma::colvec abs_vec = sum(abs(mat1 - mat2),1);
  return abs_vec;
}

