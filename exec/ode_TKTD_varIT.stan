functions {
  /* bisectioning search of the index i such that sorted[i] < x < sorted[i+1]
  this come from Sebastian Weber
  */
    
    // #include "common_functions.stan"
    
    /* for multiple .stan files */
    
    /* bisectioning search of the index i such that sorted[i] < x < sorted[i+1]
  this come from Sebastian Weber
  */
    int find_interval_elem(real x, vector sorted, int start_ind) {
      int res;
      int N;
      int max_iter;
      real left;
      real right;
      int left_ind;
      int right_ind;
      int iter;
      
      N = num_elements(sorted);
      
      if(N == 0) return(0);
      
      left_ind  = start_ind;
      right_ind = N;
      
      max_iter = 100 * N;
      left  = sorted[left_ind ] - x;
      right = sorted[right_ind] - x;
      
      if(0 <= left)  return(left_ind-1);
      if(0 == right) return(N-1);
      if(0 >  right) return(N);
      
      iter = 1;
      while((right_ind - left_ind) > 1  && iter != max_iter) {
        int mid_ind;
        real mid;
        // is there a controlled way without being yelled at with a warning?
          mid_ind = (left_ind + right_ind) / 2;
          mid = sorted[mid_ind] - x;
          if (mid == 0) return(mid_ind-1);
          if (left  * mid < 0) { right = mid; right_ind = mid_ind; }
          if (right * mid < 0) { left  = mid; left_ind  = mid_ind; }
          iter = iter + 1;
      }
      if(iter == max_iter)
        print("Maximum number of iterations reached.");
      return(left_ind);
    }
  
  
  /*    Function for linear interpolation*/
    
    real linearInterp( real t_x, real t_before, real t_after, real y_before, real y_after){
      real linInterp_hat;
      
      linInterp_hat = y_before + (t_x - t_before) * (y_after - y_before)/(t_after-t_before);
      
      return(linInterp_hat);
    }
  
  
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
    real dy_dt[1]; //
      
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
    
    return(dy_dt);
  }
  
  matrix solve_TKTD_varIT(real[] y0, real t0, real[] ts, real[] theta, real[] tconc, real[] conc){
    
    int x_i[1];
    x_i[1] = size(tconc);
    
    return(to_matrix(
      integrate_ode_rk45(TKTD_varIT, y0, t0, ts, theta, 
                         to_array_1d(append_row(to_vector(tconc), to_vector(conc))),
                         x_i,
                         // additional control parameters for the solver: real rel_tol, real abs_tol, int max_num_steps
                         10e-8, 10e-5, 1e3)));
  }
}

data {
  
  int<lower=1> n_group; // number of groups
  
  /* Concentration */
    int<lower=1> n_data_conc; // length of data for concentration
    real conc[n_data_conc]; // concentration
    real tconc[n_data_conc]; // time of concentration
    
    int<lower=1> idC_lw[n_group]; // e.g. 1 6 12 18
    int<lower=1> idC_up[n_group]; // e.g. 6 12 18 24
    
    /* Survivors */
      
    int<lower=1> n_data_Nsurv; // number of group: 4
    int Nsurv[n_data_Nsurv];
    int Nprec[n_data_Nsurv];
    
    real tNsurv[n_data_Nsurv]; // time of concentration
    
    int<lower=1> idS_lw[n_group]; // e.g. 1 6 12 18
    int<lower=1> idS_up[n_group]; // e.g. 6 12 18 24
    
    /* PRIORS */
    real hb_meanlog10;
    real hb_sdlog10;
    real kd_meanlog10;
    real kd_sdlog10;
    
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
  real hb_log10;
  real kd_log10;
  real alpha_log10;
  real beta_log10;
  
}
transformed parameters{
  
  real<lower=0> param[4]; //
  
  matrix[n_data_Nsurv, 1] y_hat;
  // vector[n_data_Nsurv] loglogistic_cdf;
  vector<lower=0, upper=1>[n_data_Nsurv] Psurv_hat;
  vector<lower=0, upper=1>[n_data_Nsurv] Conditional_Psurv_hat;
  
  param[1] = 10^hb_log10; // hb
  param[2] = 10^kd_log10; // kd
  param[3] = 10^alpha_log10; // alpha
  param[4] = 10^beta_log10; // beta
    
  for(gr in 1:n_group){
    /* initial time must be less than t0 = 0, so we use a very small close small number -1e-9 */
      y_hat[idS_lw[gr]:idS_up[gr],1] = solve_TKTD_varIT(y0, -1e-9, tNsurv[idS_lw[gr]:idS_up[gr]], param, tconc[idC_lw[gr]:idC_up[gr]], conc[idC_lw[gr]:idC_up[gr]])[,1];
  }
  
  for(gr in 1:n_group){
    for(i in idS_lw[gr]:idS_up[gr]){
      /* To compute the loglogistic_cdf, it is better to compute a loglogistic_cdf_log  */
      
      // loglogistic_cdf[i] = exp( -log1p_exp(-param[4] * (log(y_hat[i, 1]) - log(param[3]))) );
      // Psurv_hat[i] = exp( - param[1] * tNsurv[i]) * (1- loglogistic_cdf[i]);
      
      /* Compared to model block, objects in transformed parameters are saved. So we need to reduce the number of objects */
      
      Psurv_hat[i] = exp( - param[1] * tNsurv[i]) * (1- exp( -log1p_exp(-param[4] * (log(max(y_hat[idS_lw[gr]:i, 1])) - log(param[3]))) ));
      
      //Psurv_hat[i] = exp( - param[1] * tNsurv[i]) * (1- exp( -log1p_exp(-param[4] * (log(y_hat[i, 1]) - log(param[3]))) ));
      
      Conditional_Psurv_hat[i] =  i == idS_lw[gr] ? Psurv_hat[i] : Psurv_hat[i] / Psurv_hat[i-1] ;
          
    }
  }
}
model {
  
  hb_log10 ~ normal( hb_meanlog10, hb_sdlog10 );
  kd_log10 ~ normal( kd_meanlog10, kd_sdlog10 );
  alpha_log10  ~ normal( alpha_meanlog10,   alpha_sdlog10 );
  beta_log10 ~ uniform( beta_minlog10 , beta_maxlog10 );

  for(gr in 1:n_group){
    
    Nsurv[idS_lw[gr]:idS_up[gr]] ~ binomial( Nprec[idS_lw[gr]:idS_up[gr]], Conditional_Psurv_hat[idS_lw[gr]:idS_up[gr]]);
    
  }
}
generated quantities {
  int Nsurv_ppc[n_data_Nsurv];
  int Nsurv_sim[n_data_Nsurv];
  int Nsurv_sim_prec[n_data_Nsurv];
  for(gr in 1:n_group){
    /* binomial_rng function cannot be vectorized, so we need to use a loop*/
     for(i in idS_lw[gr]:idS_up[gr]){
       Nsurv_ppc[i] = binomial_rng(Nprec[i], Conditional_Psurv_hat[i]);
       
       Nsurv_sim_prec[i] = i == idS_lw[gr] ? Nprec[i] : Nsurv_sim[i-1] ;
       
       Nsurv_sim[i] = binomial_rng(Nsurv_sim_prec[i], Conditional_Psurv_hat[i]);
     }
  }
}

