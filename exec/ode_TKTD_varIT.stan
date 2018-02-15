#include "license.stan"

functions {
  
  #include "common_functions.stan"
  
  real[] TKTD_varIT( real t,
                     real[] y,
                     real[] theta,
                     real[] x_r,
                     int[]  x_i) {
    
    // - parameters
    real kd = theta[1];
    
    // - new variables
    real dy_dt[1]; //
      
      // - latent variables
    int Nconc = x_i[1]; // Number of point measuring concentration
    
    vector[Nconc] tconc = to_vector(x_r[1:Nconc]);
    vector[Nconc] conc = to_vector(x_r[Nconc+1:2*Nconc]); // to take dose time which is in ts
    
    int pulse_index = find_interval_elem(t, tconc, 1); // pulse_index index
    
    // if_else statement
    real conc_linInterp = pulse_index != 0 ? linearInterp(t , tconc[pulse_index], tconc[pulse_index+1], conc[pulse_index], conc[pulse_index+1] ) : conc[1];

    // - model
    dy_dt[1] =  kd * ( conc_linInterp - y[1]);
    
    return(dy_dt);
  }
  
  matrix solve_TKTD_varIT(real[] y0, real t0, real[] ts, real[] theta, real[] tconc, real[] conc, real[] odeParam){
    
    int x_i[1];
    x_i[1] = size(tconc);
  
    return(to_matrix(
      integrate_ode_rk45(TKTD_varIT, y0, t0, ts, theta,
                         to_array_1d(append_row(to_vector(tconc), to_vector(conc))),
                         x_i,
                         // additional control parameters for the solver: real rel_tol, real abs_tol, int max_num_steps
                         odeParam[1], odeParam[2], odeParam[3])));

  }
}

data {
  
   #include "data_guts.stan"
    
    /* PRIORS */
    real alpha_meanlog10;
    real alpha_sdlog10;
    real beta_minlog10;
    real beta_maxlog10;
}
transformed data{
  
  real<lower=0> y0[1];
  real odeParam[3];
  
  real tNsurv_ode[n_data_Nsurv]; // time of Nbr survival to include in the ode !
  real tconc_ode[n_data_conc]; // time of Nbr survival to include in the ode !
  
  y0[1] = 1e-20; // cannot start at 0 for log(0)
  
  // Add odeSolveParameters
  odeParam[1] = rel_tol;
  odeParam[2] = abs_tol;
  odeParam[3] = max_num_steps;
  
  for(gr in 1:n_group){
    tNsurv_ode[idS_lw[gr]:idS_up[gr]] = tNsurv[idS_lw[gr]:idS_up[gr]];
    tNsurv_ode[idS_lw[gr]] = tNsurv[idS_lw[gr]] + 1e-9 ; // to start ode integrator at 0
    tconc_ode[idC_lw[gr]:idC_up[gr]] = tconc[idC_lw[gr]:idC_up[gr]];
    tconc_ode[idC_lw[gr]] = tconc[idC_lw[gr]] + 1e-9 ; // to start ode integrator at 0
  }
  
}
parameters {
  
  // real hb_log10  ;
  // real kd_log10 ;
  // real alpha_log10 ;
  real beta_log10  ;
  
  real sigma[3] ;


}
transformed parameters{
  
  real hb_log10 = hb_meanlog10 + hb_sdlog10 * sigma[1] ;
  real kd_log10 = kd_meanlog10 + kd_sdlog10 * sigma[2] ;
  real alpha_log10 = alpha_meanlog10 + alpha_sdlog10 * sigma[3] ;
   
  real<lower=0> param[1]; //
  
  matrix[n_data_Nsurv, 1] y_hat;
  
  vector<lower=0, upper=1>[n_data_Nsurv] Psurv_hat;
  vector<lower=0, upper=1>[n_data_Nsurv] Conditional_Psurv_hat;
  
  real hb = 10^hb_log10; // hb
  real alpha = 10^alpha_log10; // alpha
  real beta = 10^beta_log10; // beta

  param[1] = 10^kd_log10; // kd
 
  for(gr in 1:n_group){
    /* initial time must be less than t0 = 0, so we use a very small close small number -1e-9 */
      y_hat[idS_lw[gr]:idS_up[gr], 1] = solve_TKTD_varIT(y0, 0, tNsurv_ode[idS_lw[gr]:idS_up[gr]], param, tconc_ode[idC_lw[gr]:idC_up[gr]], conc[idC_lw[gr]:idC_up[gr]], odeParam)[,1];

    for(i in idS_lw[gr]:idS_up[gr]){
      
     Psurv_hat[i] = exp(- hb * tNsurv_ode[i]) * (1-exp(loglogistic_lcdf(max(y_hat[idS_lw[gr]:i, 1]) | alpha, beta)));
     Conditional_Psurv_hat[i] =  i == idS_lw[gr] ? Psurv_hat[i] : Psurv_hat[i] / Psurv_hat[i-1] ;
     
    }

  }
}
model {

  // hb_log10 ~ normal( hb_meanlog10, hb_sdlog10 );
  // kd_log10 ~ normal( kd_meanlog10, kd_sdlog10 );
  // alpha_log10  ~ normal( alpha_meanlog10,   alpha_sdlog10 );
  
  // beta_log10 ~ uniform( beta_minlog10 , beta_maxlog10 );
  target += uniform_lpdf(beta_log10 | beta_minlog10 , beta_maxlog10 );
  
  target += normal_lpdf(sigma | 0, 1);
  // sigma ~ normal(0,1);

  for(gr in 1:n_group){
     
    target += binomial_lpmf(Nsurv[idS_lw[gr]:idS_up[gr]] | Nprec[idS_lw[gr]:idS_up[gr]], Conditional_Psurv_hat[idS_lw[gr]:idS_up[gr]]);
    // Nsurv[idS_lw[gr]:idS_up[gr]] ~ binomial( Nprec[idS_lw[gr]:idS_up[gr]], Conditional_Psurv_hat[idS_lw[gr]:idS_up[gr]]);
    
  }
}
generated quantities {
  
  #include "gen_quantities_guts.stan"
  
}

