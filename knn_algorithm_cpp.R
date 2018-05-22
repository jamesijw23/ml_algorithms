library(RcppArmadillo)
library(inline)


##-----------------------------------
## Name: repbyrow
## Input: 
  ## a) v1 --> Vector of values
  ## b) num_row --> rnumber of rows repeat
## Output: Matrix of vector repeated by row
## Purpose: Repeat a vector into num_row times by rows
## Improvements: 
  ## a) No for loop 
##-----------------------------------
cppFunction(depends=c('RcppArmadillo','Rcpp'),'arma::mat repbyrow(arma::vec v1,int num_row){
            arma::vec t_v1 = arma::trans(v1);
            arma::mat matrix_v1(v1.size(),num_row);
            for(int i=0;i<num_row;i++){
            matrix_v1.row(i) = t_v1;
            }
            return matrix_v1;
            }')


##-----------------------------------
## Name: euclidean_dist
## Input: 
  ## a) Two vectors of same length
## Output: Numeric distance between two vectors
## Purpose: Euclidean Distance per Vector
## Improvements:  
##-----------------------------------
cppFunction(depends=c('RcppArmadillo','Rcpp'),
           'double euclidean_dist(arma::vec x1,arma::vec x2) {
            int N = x1.n_rows;
            double total = 0;
            arma::vec x3(N);
            x3 = pow(x1 - x2, 2);
            total = sqrt(sum(x3));
            return total;
            }')

##-----------------------------------
## Name: euclidean_dist_matrix
## Input:
  ## a) mat1 --> matrix of data
  ## b) x1 --> vector
## Output: Euclidean distance between vector and each row vector in matrix
## Purpose: Euclidean Distance per Matrix
## Improvements:  
##
##-----------------------------------
cppFunction(depends=c('RcppArmadillo','Rcpp'),
            'double euclidean_dist_matrix(arma::mat mat1,arma::vec x1) {
            int num_rows = mat1.n_rows;
            arma::mat mat2 = repbycol(x1,num_rows);
            return 2.0;
            }')
 
##-----------------------------------
## Name: 
## Input: 
## Output: 
## Purpose:
## Improvements:  
## Absolute Distance per Vector
##-----------------------------------
cppFunction(depends=c('RcppArmadillo','Rcpp'),
            'double absolute_dist(arma::vec x1,arma::vec x2) {
            int N = x1.n_rows;
            double total = 0;
            arma::vec x3(N);
            x3 = abs(x1 - x2);
            total = sum(x3);
            return total;
            }')




