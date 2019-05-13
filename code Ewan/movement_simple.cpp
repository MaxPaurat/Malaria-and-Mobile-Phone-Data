// Simple movement model, no temporal data
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_INTEGER(Nadmin);
  DATA_VECTOR(log_relPops); // length: Nadmin
  DATA_MATRIX(log_distMatrix); // dim: Nadmin x Nadmin
  DATA_INTEGER(Nobs);
  DATA_IMATRIX(journeyMatrix); // dim: Nobs x 2
  
  PARAMETER(distance_index);
  PARAMETER(pop_index);
  
  Type nll = 0;

  // Priors
  
  nll -= dnorm(distance_index,Type(0),Type(0.5),true);
  nll -= dnorm(pop_index,Type(0),Type(0.5),true);
  
  // Construct Journey Probability Matrix
  
  matrix<Type> log_journeyProbMatrix(Nadmin,Nadmin);
  log_journeyProbMatrix = log_distMatrix.array()*distance_index;
  for (int i=0; i<Nadmin; i++) {
    for (int j=0; j<Nadmin; j++) {
      log_journeyProbMatrix(i,j) += log_relPops[j]*pop_index;
    }
  }
  Type total;
  for (int i=0; i<Nadmin; i++) {
    total = 0.0;
    for (int j=0; j<Nadmin; j++) {
      if (j!=i) {total += exp(log_journeyProbMatrix(i,j));}
    }
    for (int j=0; j<Nadmin; j++) {
      log_journeyProbMatrix(i,j) -= log(total);
    }
  }

  // Likelihood
  for (int i=0; i<Nobs; i++) {
    nll -= log_journeyProbMatrix(journeyMatrix(i,0)-1,journeyMatrix(i,1)-1);
  }

  // Reportings
  REPORT(log_journeyProbMatrix);
  
  return nll;
}
