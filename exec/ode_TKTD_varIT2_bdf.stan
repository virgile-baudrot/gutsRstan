#include "license.stan"

functions {
  
  #include "common_functions.stan"
  
  real[] TKTD_varIT( real t,
                     real[] y,
                     real[] theta,
                     real[] x_r,
                     int[]  x_i) {
    
    // - parameters
    real hb = theta[1];
    real kd = theta[2];
    real alpha = theta[3];
    real beta = theta[4];
    
    // - new variables
    real dy_dt[2]; //
      
      // - latent variables
    int Nconc = x_i[1]; // Number of point measuring concentration
    
    vector[Nconc] tconc = to_vector(x_r[1:Nconc]);
    vector[Nconc] conc = to_vector(x_r[Nconc+1:2*Nconc]); // to take dose time which is in ts
    
    int pulse_index = find_interval_elem(t, tconc, 1); // pulse_index index
    
    // if_else statement
    real conc_linInterp = pulse_index != 0 ? linearInterp(t , tconc[pulse_index], tconc[pulse_index+1], conc[pulse_index], conc[pulse_index+1] ) : conc[1];
    //real conc_linInterp = linearInterp(t , tconc[pulse_index], tconc[pulse_index+1], conc[pulse_index], conc[pulse_index+1] );
    
    // - model
    dy_dt[1] =  kd * ( conc_linInterp - y[1]);
    
    dy_dt[2] = ( (-beta*(y[1] / alpha)^(beta-1)) / (alpha * (1+ (y[1]/alpha)^(beta))^2) ) * (exp(-hb*t) * dy_dt[1]) - (hb * exp(-hb*t))/(1+(y[1]/alpha)^beta)   ;
    
    return(dy_dt);
  }
  
  matrix solve_TKTD_varIT2(real[] y0, real t0, real[] ts, real[] theta, real[] tconc, real[] conc, real[] odeParam){
    
    int x_i[1];
    
    x_i[1] = size(tconc);
    
    return(to_matrix(
      integrate_ode_rk45(TKTD_varIT, y0, t0, ts, theta,
                        to_array_1d(append_row(to_vector(tconc), to_vector(conc))),
                        x_i,
                        // additional control parameters for the solver: real rel_tol, real abs_tol, int max_num_steps
                        odeParam[1], odeParam[2], odeParam[3])));
    
    // DO NOT COMPIL WITH THE ode_bdf !
    // return(to_matrix(
    //   integrate_ode_bdf(TKTD_varIT, y0, t0, ts, theta,
    //                     to_array_1d(append_row(to_vector(tconc), to_vector(conc))),
    //                     x_i,
    //                     // additional control parameters for the solver: real rel_tol, real abs_tol, int max_num_steps
    //                     odeParam[1], odeParam[2], odeParam[3])));
    
    
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
  
  real<lower=0> y0[2];
  real odeParam[3];
  
  y0[1] = 0;
  y0[2] = 1; // refers to Survival Rate
  
  // Add odeSolveParameters
  odeParam[1] = rel_tol;
  odeParam[2] = abs_tol;
  odeParam[3] = max_num_steps;
  
}
parameters {
  real hb_log10;
  real kd_log10;
  real alpha_log10;
  real beta_log10;
  
  //real<lower=0> y0[2];
  
}
transformed parameters{
  
  real<lower=0> param[4]; //
    
    matrix[n_data_Nsurv,2] y_hat;
  vector[n_data_Nsurv] Psurv_hat;
  vector[n_data_Nsurv] Conditional_Psurv_hat_noPos;
  vector<lower=0, upper=1>[n_data_Nsurv] Conditional_Psurv_hat;
  
  param[1] = 10^hb_log10; // hb
  param[2] = 10^kd_log10; // kd
  param[3] = 10^alpha_log10; // alpha
  param[4] = 10^beta_log10; // beta
  
  for(gr in 1:n_group){
    /* initial time must be less than t0 = 0, so we use a very small close small number -1e-9 */
      y_hat[idS_lw[gr]:idS_up[gr],1:2] = solve_TKTD_varIT2(y0, -1e-9, tNsurv[idS_lw[gr]:idS_up[gr]], param, tconc[idC_lw[gr]:idC_up[gr]], conc[idC_lw[gr]:idC_up[gr]], odeParam);
      
      Psurv_hat[idS_lw[gr]:idS_up[gr]] = y_hat[idS_lw[gr]:idS_up[gr], 2];
      
      for(i in idS_lw[gr]:idS_up[gr]){
        
        Conditional_Psurv_hat_noPos[i] =  i == idS_lw[gr] ? Psurv_hat[i] : Psurv_hat[i] / Psurv_hat[i-1] ;
        // Ensure positivity of Conditional probability
        Conditional_Psurv_hat[i] = Conditional_Psurv_hat_noPos[i] < 0 ? 0 : Conditional_Psurv_hat_noPos[i] ;
        
      }
  }
}
model {
  
  hb_log10 ~ normal( hb_meanlog10, hb_sdlog10 );
  kd_log10 ~ normal( kd_meanlog10, kd_sdlog10 );
  alpha_log10  ~ normal( alpha_meanlog10, alpha_sdlog10 );
  beta_log10 ~ uniform( beta_minlog10, beta_maxlog10 );
  
  // y0 ~ exponential(1e9);
  
  for(gr in 1:n_group){
    
    Nsurv[idS_lw[gr]:idS_up[gr]] ~ binomial( Nprec[idS_lw[gr]:idS_up[gr]], Conditional_Psurv_hat[idS_lw[gr]:idS_up[gr]]);
    
  }
}
generated quantities {
  
  #include "gen_quantities_guts.stan"
  
}

