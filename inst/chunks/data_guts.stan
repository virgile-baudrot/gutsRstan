// Number of groups
int<lower=1> n_group; 
  
// Concentration
int<lower=1> n_data_conc; // length of data for concentration
real conc[n_data_conc]; // concentration
real tconc[n_data_conc]; // time of concentration

int<lower=1> idC_lw[n_group]; // e.g. 1 6 12 18
int<lower=1> idC_up[n_group]; // e.g. 6 12 18 24

// Survivors
int<lower=1> n_data_Nsurv; // number of group: 4
int Nsurv[n_data_Nsurv];
int Nprec[n_data_Nsurv];
real tNsurv[n_data_Nsurv]; // time of Nbr survival

int<lower=1> idS_lw[n_group]; // e.g. 1 6 12 18
int<lower=1> idS_up[n_group]; // e.g. 6 12 18 24

// PRIORS
real hb_meanlog10;
real hb_sdlog10;
real kd_meanlog10;
real kd_sdlog10;

// Parameters for integration of differentiol equations
real rel_tol;
real abs_tol;
int max_num_steps;
