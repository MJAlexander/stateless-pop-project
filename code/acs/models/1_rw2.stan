data {
  int<lower=0> N;
  int<lower=0> S;
  int<lower=0> P;
  matrix[N,S] y;
  matrix[N,S] se_y;
}
parameters {
  real<lower=0> sigma;
  //real<lower=0> tau;
  //real mu_sig;
  matrix[N,S] eps;
}

transformed parameters {
  matrix[N,S] lambda;
  //vector[S] log_sigma;
  for(i in 1:N){
    for(s in 1:S){
      lambda[i,s] = eps[i,s];
    }
  }
  
  //log_sigma = log(sigma);
}


model {
  
 

  //log_sigma ~ normal(mu_sig,tau);
  //tau ~ normal(0,1);
  //mu_sig ~ normal(0,1);
  sigma ~ normal(0,1);
  
  for(s in 1:S){
     eps[1,s] ~ normal(y[1,s], sigma);
     eps[2,s] ~ normal(eps[1,s], sigma);
     eps[3:N,s] ~ normal(2*eps[2:(N - 1),s] - eps[1:(N - 2),s], sigma);
     y[,s] ~ normal(eps[,s], se_y[,s]); 
  }
}

generated quantities{
  //project forward P years
  matrix[P,S] eps_p;
  matrix[P,S] y_p;
  
  for(s in 1:S){
      eps_p[1,s] = normal_rng(2*eps[N,s] - eps[N-1,s], sigma);
      y_p[1,s] = normal_rng(eps_p[1,s], se_y[N,s]); 
      eps_p[2,s] = normal_rng(2*eps_p[1,s] - eps[N, s], sigma);
      y_p[2,s] = normal_rng(eps_p[2,s], se_y[N,s]); 
    for(p in 3:P){
     eps_p[p,s] = normal_rng(2*eps_p[p-1,s] - eps_p[p-2,s], sigma);
     y_p[p,s] = normal_rng(eps_p[p,s], se_y[N,s]); 
    }

  }
  
}