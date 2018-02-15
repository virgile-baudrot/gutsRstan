#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4ode_TKTD_varCOMBINED_mod) {


    class_<rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> >("model_ode_TKTD_varCOMBINED")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_ode_TKTD_varCOMBINED_namespace::model_ode_TKTD_varCOMBINED, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4ode_TKTD_varIT2_mod) {


    class_<rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> >("model_ode_TKTD_varIT2")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_ode_TKTD_varIT2_namespace::model_ode_TKTD_varIT2, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4ode_TKTD_varIT2_bdf_mod) {


    class_<rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> >("model_ode_TKTD_varIT2_bdf")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_ode_TKTD_varIT2_bdf_namespace::model_ode_TKTD_varIT2_bdf, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4ode_TKTD_varIT_mod) {


    class_<rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> >("model_ode_TKTD_varIT")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_ode_TKTD_varIT_namespace::model_ode_TKTD_varIT, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4ode_TKTD_varPROPER_loglogistic_mod) {


    class_<rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> >("model_ode_TKTD_varPROPER_loglogistic")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_ode_TKTD_varPROPER_loglogistic_namespace::model_ode_TKTD_varPROPER_loglogistic, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4ode_TKTD_varPROPER_lognormal_mod) {


    class_<rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> >("model_ode_TKTD_varPROPER_lognormal")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_ode_TKTD_varPROPER_lognormal_namespace::model_ode_TKTD_varPROPER_lognormal, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4ode_TKTD_varSD_mod) {


    class_<rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> >("model_ode_TKTD_varSD")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_ode_TKTD_varSD_namespace::model_ode_TKTD_varSD, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
