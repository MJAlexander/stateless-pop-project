
data {
  
  // shared constants -------------
  int<lower=1> K_r; // number of risk levels
  int<lower=1> K_bp; // number of birthplaces
  int<lower=1> K_a; // number of regions
  
  // data for successes ------------
  int<lower=1> N_y;
  array[N_y] int<lower=0, upper=1> y;
  
  array[N_y] int<lower=1, upper=K_r> r_y;
  array[N_y] int<lower=1, upper=2> r_binary_y;
  array[N_y] int<lower=1, upper=K_bp> bp_y;
  matrix<lower=0, upper=1>[N_y, K_bp] X_bp;
  array[N_y] int<lower=1, upper=K_a> a_y; 
  
  real<lower=0> scale_b_0;
  
  // data for risk level ------------
  int<lower=1> N_r;
  array[N_r, K_r] int<lower=0> r;
  
  array[N_r] int<lower=1, upper=K_bp> bp_r;
  array[N_r] int<lower=1, upper=K_a> a_r;
  
  real<lower=0> scale_u_0;
  
  // for full estimates ------
  int<lower=1> N_pred;
  array[N_pred] int<lower=1, upper= K_bp> bp_pred;
  array[N_pred] int<lower=1, upper = K_a> a_pred;
  vector<lower=0>[N_pred] pop_pred;
}

transformed data {
}

parameters {
  
  // for successes model -------------
  real b_0_raw;
  
  vector[K_bp] b_bp_raw;
  real<lower=0> sd_b_bp;
  
  vector[K_r] b_r_binary;
  
//  matrix[2, K_bp] b_bp_r_raw; // random slopes by binary risk level
//  vector[2] sd_b_bp_r; 
  
  vector[K_a] b_a_raw;
  real<lower=0> sd_b_a;
  
  // for risk level model ----------------
  row_vector[K_r] u_0_raw; 
  
  matrix[K_bp, K_r] u_bp_raw;
  array[K_r] real<lower=0> sd_u_bp;

  matrix[K_a, K_r] u_a_raw;
  array[K_r] real<lower=0> sd_u_a;
}

transformed parameters {
  
  // for successes model ----------
  real b_0 = scale_b_0*b_0_raw;
  vector[K_bp] b_bp = b_bp_raw*sd_b_bp;
  vector[K_a] b_a = b_a_raw*sd_b_a;
  
  // interaction term
//  vector[N_y] b_bp_r_y = rows_dot_product(X_bp, diag_pre_multiply(sd_b_bp_r, b_bp_r_raw)[r_binary_y]);
  
  vector[N_y] mu_y = b_0 + b_r_binary[r_binary_y] + b_bp[bp_y] +  b_a[a_y]; // + b_bp_r_y 
  
  // for risk level model ------------
  row_vector[K_r] u_0 = scale_u_0*u_0_raw;
  matrix[K_bp, K_r] u_bp;
  matrix[K_a, K_r] u_a;
  
  matrix[N_r, K_r] mu_r;
  
  for (k in 1:K_r) {
    u_bp[,k] = u_bp_raw[,k]*sd_u_bp[k];
  }
  
  for (k in 1:K_r) {
    u_a[,k] = u_a_raw[,k]*sd_u_a[k];
  }

  mu_r = rep_matrix(u_0, N_r) + u_bp[bp_r] +  u_a[a_r];

}

model {

  // Model for procedure success ----------
  y ~ bernoulli_logit(mu_y);
  b_0_raw ~ std_normal();
  b_bp_raw ~ std_normal();
  sd_b_bp ~ std_normal();

  b_r_binary ~ std_normal();
  
//  for (k in 1:2) {
//    b_bp_r_raw[k] ~ std_normal();
//  }
//  sd_b_bp_r ~ std_normal();

  b_a_raw ~ std_normal();
  sd_b_a ~ std_normal();
  
  // risk level model ---------------
  for (n in 1:N_r) {
    r[n] ~ multinomial_logit(to_vector(mu_r[n]));
  }
  
  u_0_raw ~ std_normal();
  
  for (k in 1:K_r) {
    u_a_raw[, k] ~ std_normal();
  }
  
  for(k in 1:K_r) {
    u_bp_raw[, k] ~ std_normal();
  }
  
  sd_u_a ~ std_normal();
  sd_u_bp ~ std_normal();
  
}

generated quantities {

  matrix[N_pred, K_r] mu_r_pred;
  matrix<lower=0, upper=1>[N_pred, K_r] r_props_pred;
  matrix<lower=0>[N_pred, K_r] r_counts;
  
  matrix[N_pred, K_r] mu_pred;
  matrix<lower=0, upper=1>[N_pred, K_r] unweighted_p;
  
  matrix<lower=0, upper=1>[N_pred, K_r] p_pred;
  matrix<lower=0>[N_pred, K_r] counts_pred;
  vector<lower=0>[K_r] pop_by_risk_level;
  vector<lower=0>[K_r] counts_by_risk_level;
  
  real<lower=0> total_count_pred;

  // Predictions of risk level 
  mu_r_pred = rep_matrix(u_0, N_pred) + u_bp[bp_pred] + u_a[a_pred];
  
  for (n in 1:N_pred) {
    r_props_pred[n] = softmax(mu_r_pred[n]')';
  }
  
  r_counts = diag_pre_multiply(pop_pred, r_props_pred);
  
  // population by risk level
  for (k in 1:K_r) {
    pop_by_risk_level[k] = sum(r_counts[,k]);
  }
  
  // Predictions of procedure outcome based on risk level and birthplace
  mu_pred = rep_matrix(b_0 + b_bp[bp_pred] + b_a[a_pred], K_r) + 
    rep_matrix(b_r_binary[{1,2,2}]', N_pred);// + 
  //  diag_pre_multiply(sd_b_bp_r, b_bp_r_raw)[{1,2,2}]'[bp_pred];
    
  unweighted_p = inv_logit(mu_pred);
  
  // Assume that those in the "no risk" category cannot be stateless
  unweighted_p[,3] = rep_vector(0, N_pred);
  
  // Multiply the risk level proportions by the failure proportion
  p_pred = r_props_pred .* unweighted_p;

  // Obtain counts by subgroup
  counts_pred = diag_pre_multiply(pop_pred, p_pred);

  // outcomes by risk level
  for (k in 1:K_r) {
    counts_by_risk_level[k] = sum(counts_pred[,k]);
  }
  
  // Get total count
  total_count_pred = sum(counts_by_risk_level);
  
}
