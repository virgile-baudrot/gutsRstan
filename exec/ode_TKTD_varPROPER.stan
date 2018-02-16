#include "license.stan"

functions {

   #include "common_functions.stan"


  real[] TKTD_varPROPER( real t,
                     real[] y,
                     real[] theta,
                     real[] x_r,
                     int[]  x_i) {

    // - parameters
    real hb = theta[1];
    real kd = theta[2];
    real z = theta[3];
    real kk = theta[4];
    
    // - new variables
    real max_z[2]; // 
    real dy_dt[2]; //

    // - latent variables
    int Nconc = x_i[1]; // Number of point measuring concentration
    
    vector[Nconc] tconc = to_vector(x_r[1:Nconc]);
    vector[Nconc] conc = to_vector(x_r[Nconc+1:2*Nconc]); // to take dose time which is in ts
    
    int pulse_index = find_interval_elem(t, tconc, 1); // pulse_index index
    
    // if_else statement
    real conc_linInterp = pulse_index != 0 ? linearInterp(t , tconc[pulse_index], tconc[pulse_index+1], conc[pulse_index], conc[pulse_index+1] ) : conc[1];
    
    // - model
    dy_dt[1] =  kd * ( conc_linInterp - y[1]);

    max_z[1] = 0;
    max_z[2] = y[1] - z;

    dy_dt[2] = kk * max(max_z) + hb;

    return(dy_dt);
  }
  
  matrix solve_TKTD_varPROPER(real[] y0, real t0, real[] ts, real[] theta, real[] tconc, real[] conc, real[] odeParam){

    int x_i[1];
    x_i[1] = size(tconc);
    
    return(to_matrix(
      integrate_ode_rk45(TKTD_varPROPER, y0, t0, ts, theta, 
                         to_array_1d(append_row(to_vector(tconc), to_vector(conc))),
                         x_i,
                         // additional control parameters for the solver: real rel_tol, real abs_tol, int max_num_steps
                          odeParam[1], odeParam[2], odeParam[3])));
  }
}

data {
  
  #include "data_guts.stan"
    
  int proper_distribution;  
  /* PRIORS */
  
  
  real kk_meanlog10;
  real kk_sdlog10;
  
  real alpha_meanlog10;
  real alpha_sdlog10;
  real beta_minlog10;
  real beta_maxlog10;
}
transformed data{

  real<lower=0> y0[2];

  y0[1] = 0;
  y0[2] = 0;
  
}
parameters {
  
  real sigma[4];
  real beta_log10;
  
  real z; // create parameter z

}
transformed parameters{

  real hb_log10 = hb_meanlog10 + hb_sdlog10 * sigma[1] ;
  real kd_log10 = kd_meanlog10 + kd_sdlog10 * sigma[2] ;
  real alpha_log10 = alpha_meanlog10 + alpha_sdlog10 * sigma[3] ;
  real kk_log10 = kk_meanlog10 + kk_sdlog10 * sigma[4] ;

  real<lower=0> param[4]; //

  matrix[n_data_Nsurv, 2] y_hat;
  vector<lower=0, upper=1>[n_data_Nsurv] Psurv_hat;
  vector<lower=0, upper=1>[n_data_Nsurv] Conditional_Psurv_hat;
  
  param[1] = 10^hb_log10; // hb
  param[2] = 10^kd_log10; // kd
  param[3] = z;
  param[4] = 10^kk_log10; // kk
  
  for(gr in 1:n_group){
  /* initial time must be less than t0 = 0, so we use a very small close small number -1e-8 */
    y_hat[idS_lw[gr]:idS_up[gr],1:2] = solve_TKTD_varPROPER(y0, 0, tNsurv_ode[idS_lw[gr]:idS_up[gr]], param, tconc_ode[idC_lw[gr]:idC_up[gr]], conc[idC_lw[gr]:idC_up[gr]], odeParam);
    
    Psurv_hat[idS_lw[gr]:idS_up[gr]] = exp( - y_hat[idS_lw[gr]:idS_up[gr], 2]);
    
    for(i in idS_lw[gr]:idS_up[gr]){
  
      Conditional_Psurv_hat[i] =  i == idS_lw[gr] ? Psurv_hat[i] : Psurv_hat[i] / Psurv_hat[i-1] ;
  
    }
  }

}
model{
  
  target += normal_lpdf(sigma | 0, 1);
  
  target += uniform_lpdf(beta_log10 | beta_minlog10 , beta_maxlog10 );
  
  if(proper_distribution == 1){
    target += loglogistic_lpdf(z | 10^alpha_log10, 10^beta_log10)
  }
  if(proper_distribution == 2){
    target += lognormal_lpdf(z | 10^alpha_log10, 10^beta_log10)
  }

  for(gr in 1:n_group){
     target += binomial_lpmf(Nsurv[idS_lw[gr]:idS_up[gr]] | Nprec[idS_lw[gr]:idS_up[gr]], Conditional_Psurv_hat[idS_lw[gr]:idS_up[gr]]);
  }
}
generated quantities {
  
  #include "gen_quantities_guts.stan"
  
}

