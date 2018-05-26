library(Rcpp)
library(rbenchmark)

cppFunction('
int g(int n) { if (n < 2)
return(n); 
return(g(n-1) + g(n-2)); }')

f <- function(n) {
  if (n < 2) return(n)
  return(f(n-1) + f(n-2))
}

sapply(0:10, f)

benchmark(f(20), g(20))[,1:4]
