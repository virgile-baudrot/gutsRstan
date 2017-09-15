data {
  int<lower=1> nData; // number of data
  int N_obs[nData]; 
}
parameters {
  real a; // attack rate
  real<lower=0> sigma;
}
model {
  a ~ gamma(1,1);
  for(i in 1:nData){
      N_obs[i] ~ neg_binomial_2(a, sigma);
  }
}
