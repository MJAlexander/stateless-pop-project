data {
  int<lower=0> N; //number of obs
  int<lower=0> P; //number of obs
  vector[N] y; //obs
  vector[N] se_y; //std error
}

parameters {
  vector[N] eps;
  real<lower=0> sigma;
}

transformed parameters {
}


model {
  y ~ normal(eps, se_y);
  eps[1] ~ normal(y[1], sigma);
  eps[2] ~ normal(eps[1], sigma);
  eps[3:N] ~ normal(2*eps[2:(N - 1)] - eps[1:(N - 2)], sigma);
}

generated quantities{
  //project forward P years
  vector[P] eps_p;
  vector[P] y_p;
  
  eps_p[1] = normal_rng(2*eps[N] - eps[N-1], sigma);
  y_p[1] = normal_rng(eps_p[1], se_y[N]); 
  eps_p[2] = normal_rng(2*eps_p[1] - eps[N], sigma);
  y_p[2] = normal_rng(eps_p[2], se_y[N]); 
  for(p in 3:P){
    eps_p[p] = normal_rng(2*eps_p[p-1] - eps_p[p-2], sigma);
    y_p[p] = normal_rng(eps_p[p], se_y[N]); 
  }
  
}