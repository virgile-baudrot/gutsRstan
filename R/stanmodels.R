# This file is only intended to be used during the installation process
# nocov start
MODELS_HOME <- "exec"

if (!file.exists(MODELS_HOME)) MODELS_HOME <- sub("R$", "exec", getwd())

stan_files <- dir(MODELS_HOME, pattern = "stan$", full.names = TRUE)

stanmodels <- sapply(stan_files, function(f) {
  model_cppname <- sub("\\.stan$", "", basename(f))
  isystem <- system.file("chunks", package = getPackageName(environment(), FALSE))
  if (!file.exists(file.path(isystem, "common_functions.stan")))
    isystem <- file.path("inst", "chunks")
  if (!file.exists(file.path(isystem, "common_functions.stan")))
    isystem <- file.path("..", "inst", "chunks")
  stanfit <- rstan::stanc_builder(f, isystem, allow_undefined = TRUE)
  stanfit$model_cpp <- list(model_cppname = stanfit$model_name, 
                            model_cppcode = stanfit$cppcode)
  return(do.call(new, args = c(stanfit[-(1:3)], Class = "stanmodel", 
                 mk_cppmodule = function(x) get(paste0("model_", model_cppname)))))
  }
)
names(stanmodels) <- sub("\\.stan$", "", basename(names(stanmodels)))
rm(MODELS_HOME)
# nocov end
