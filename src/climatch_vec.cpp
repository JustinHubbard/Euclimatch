#include <Rcpp.h>
using namespace Rcpp;

//' Euclidean climatch scores
//'
//' Vector of the climatch scores within the recipient region
//'
//' @param recipient List of data.frames of the recipient regions
//' @param source List of data.frames of the source regions
//' @param globvar Vector of the global variance of each variable
//'
//' @return Vector of climatch scores corresponding to each grid cell within recipient
//' @export
//' @examples
//' i <- as.data.frame(matrix(runif(n=180, min=1, max=20), nrow=60)) # Fake source climate data.
//' j <- as.data.frame(matrix(runif(n=300, min=10, max=40), nrow=100)) # Fake recipient data.
//' variance <- c(600, 800, 450) # Fake global variance
//'
//' climatch_vec(recipient = j, source = i, globvar = variance)
// [[Rcpp::export]]
NumericVector climatch_vec(DataFrame recipient, DataFrame source, NumericVector globvar){
  NumericMatrix jarea = internal::convert_using_rfunction(recipient, "as.matrix");
  NumericMatrix iarea = internal::convert_using_rfunction(source, "as.matrix");

  double var = globvar.size();
  int mn = iarea.nrow();
  int jn = jarea.nrow();

  NumericVector cs(11);
  NumericVector jmax(jn);

  for(int j=0; j<jn; j++){
    double imin = 0;
    NumericVector biosum(mn);
    NumericVector ja = jarea(j,_);
    for(int i=0; i<mn; i++){
      double biovar = 0.0;
      NumericVector ia = iarea(i,_);
      for(int m=0; m<var; m++){
        biovar = (pow((ia[m] - ja[m]),2)/globvar[m]) + biovar;
      }
      biosum[i] = sqrt(1/var*biovar);
    }
    imin = min(biosum);
    jmax[j] = (1-imin)*10;
  }

  return jmax;
}
