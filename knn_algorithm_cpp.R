library(RcppArmadillo)
library(inline)

cppFunction(depends=c('RcppArmadillo','Rcpp'),
           'double euclidean_dist(arma::vec x1,arma::vec x2) {
            int N = x1.n_rows;
            double total = 0;
            arma::vec x3(N);
            x3 = pow(x1 - x2, 2);
            total = sqrt(sum(x3));
            return total;
            }')
cppFunction(depends=c('RcppArmadillo','Rcpp'),
            'double absolute_dist(arma::vec x1,arma::vec x2) {
            int N = x1.n_rows;
            double total = 0;
            arma::vec x3(N);
            x3 = abs(x1 - x2);
            total = sum(x3);
            return total;
            }')


