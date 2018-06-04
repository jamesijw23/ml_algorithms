#include <Rcpp.h>
using namespace Rcpp;

class Location{
public:
  Location();
  Location(int, double);
  int x;
  double y;
  void print();
};

// http://www.deanbodenham.com/learn/rcpp-classes-part-1.html
// http://www.deanbodenham.com/learn/rcpp-classes-part-2.html
// http://www.deanbodenham.com/learn/rcpp-classes-part-3.html
// http://www.deanbodenham.com/learn/rcpp-classes-part-4.html
// http://www.deanbodenham.com/learn/rcpp-classes-part-5.html
//constructors
Location::Location() :x(0), y(0) { }
Location::Location(int xi, double yi) :x(xi), y(yi) { }

//print function
void Location::print(){
  Rcpp::Rcout << "x = " << x << std::endl;
  Rcpp::Rcout << "y = " << y << std::endl;
}

RCPP_MODULE(locationmodule){
  Rcpp::class_<Location>( "Location" )
  .constructor("documentation for default constructor")
  .constructor<int,double>("documentation for constructor")
  .field( "x", &Location::x, "documentation for x")
  .field( "y", &Location::y, "documentation for y")
  .method( "print", &Location::print, "documentation for print")
  ;
}