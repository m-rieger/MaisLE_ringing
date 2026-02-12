// zip model with geographical autocorrelation

functions {
  // function to compute the covariance matrix of the Gaussian Process
  matrix cov_GPL2(matrix x, real sq_alpha, real sq_rho, real delta) {
    int N = dims(x)[1];
    matrix[N,N] K;
    for (i in 1:(N-1)) {
      K[i, i] = sq_alpha + delta;
      for (j in (i + 1):N) {
        K[i,j] = sq_alpha * exp(-sq_rho * square(x[i,j]) );
        K[j,i] = K[i,j];
      }
    }
    K[N,N] = sq_alpha + delta;
    return K;
  }
}

data {
  int<lower=0> N; // number of data items
  // int<lower=0> Nsp; // number of species
  int<lower=0> Nyear; // number of years
  int<lower=0> Nsite; // number of sites
  int<lower=0> Nland; // number of land parameter
  // integers need to be defined as arrays
  array[N] int<lower=0> y; // number of inds per day and net
  // array[N] int<lower=0> sp; // species identifier
  array[N] int<lower=0> year; // years identifier
  array[N] int<lower=0> site; // site identifier
  // vectors are real arrays by default
  matrix[N,Nland] land; // distance to field border, prop. of maize, prop. of woods, height, weed
  vector[N] effort; // effort per day and net
  vector[N] jday; // julian day

  matrix[Nsite, Nsite] Dmat; // distance matrix between sites

}

parameters { // log-transformed scale
  matrix[1,Nyear] alpha; // year and species-specific intercepts
  matrix[1,Nland] beta; // species-specific slope for dist, propM, propW, height, weed
  matrix[1,2] gamma; // species-specific slopes for julian day
  matrix[1,2] delta; // log effort slope, poly2

  // vector<lower=0, upper=1>[1] theta; // needed for species-specific zi-intercept
  
  // matrix[1, Nsite] z; // 
  vector[Nsite] z; // 
  real<lower=0> etasq; // 
  real<lower=0> rhosq; //
  

}

transformed parameters{
  // some problems with this, do not know, where it comes from
  // matrix[1, Nsite] k;
  // matrix[Nsite,Nsite] L_SIGMA;
  // matrix[Nsite,Nsite] SIGMA;
  // SIGMA = cov_GPL2(Dmat, etasq, rhosq, 0.01);
  // L_SIGMA = cholesky_decompose(SIGMA);
  // for(s in 1:1) k[s,] = L_SIGMA * z[s,];
  
  vector[Nsite] k;
  matrix[Nsite,Nsite] L_SIGMA;
  matrix[Nsite,Nsite] SIGMA;
  SIGMA = cov_GPL2(Dmat, etasq, rhosq, 0.01);
  L_SIGMA = cholesky_decompose(SIGMA);
  k = L_SIGMA * z;
  
  // needed if more than one predictor for zi
  // vector[1] theta ;
  // for(i in 1:1) theta[i] =  inv_logit(alpha_theta + sp_theta[i]) ; //<lower=0, upper=1>
  
  vector[N] lambda; // expected number of inds per day and net
  for(n in 1:N) {
    lambda[n] = alpha[1, year[n]] + // species-specific intercept per year
    beta[1,1]*land[n,1] + // species-specific slopes of distance
    beta[1,2]*land[n,2] + // species-specific slopes of propM
    beta[1,3]*land[n,3] + // species-specific slopes of propW
    beta[1,4]*land[n,4] + // species-specific slopes of weed
    //beta[1,5]*land[n,5] + // species-specific slopes of height // land = 5
    gamma[1,1]*jday[n] + // linear effect of julian day
    gamma[1,2]*jday[n]*jday[n] + // quadratic effect of julian day
    delta[1,1]*effort[n] + // linear slope of effort
    delta[1,2]*effort[n]*effort[n] + // quadratic slope of effort
   //delta[1,3]*effort[n]*effort[n]*effort[n] + // tertiar slope of effort
    k[site[n]];  // random site-specific intercept per species

  }
}

model {

  // PRIORS (to_vector for matrices)
  to_vector(alpha) ~ normal(0, 1); // prior on species-specific intercepts
  to_vector(beta)  ~ normal(0, 1); // prior on slope
  to_vector(gamma) ~ normal(0, 1); // prior on julian day slope
  to_vector(delta) ~ normal(0, 1); // prior on effort slope


  z ~ std_normal(); // don't change! ####
  rhosq ~ exponential(0.5); // prior on rho, former 0.5 ####
  etasq ~ exponential(2); // prior on eta ####
  
  // alpha_theta ~ normal(0,1);
  // sp_theta    ~ normal(0,1);
  // ggf beta prior für theta

  // LIKELIHOOD
  for (n in 1:N) {

    // if (y[n] == 0) {
    //   target += log_sum_exp(log(theta[1]), log1m(theta[1]) + poisson_log_lpmf(y[n] | lambda[n]));
    // } else {
    //   target += log1m(theta[1]) + poisson_log_lpmf(y[n] | lambda[n]);
    // }
    // target += poisson_log_lpmf(y[n] | lambda[n]); // would work also
    y[n] ~ poisson_log(lambda[n]);
  }
}

generated quantities{
  array[N] int<lower=0> y_sim;
  // int zero;
  
  for(n in 1:N){
	     // zero = bernoulli_rng(theta[1]);
	     // y_sim[n] = (1-zero)*poisson_log_rng(lambda[n]);
	     y_sim[n] = poisson_log_rng(lambda[n]);
  }
}
