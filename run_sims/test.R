# Code to run the model

# Baseline parameter values and objects
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
iter0 <- 10
rounds0 <- 60

# Object changes from defaults for the current simulations
# stcdv0 <- 2.5
# u0 <- .6
# ths0 <- 1.1
# btp <- 5
# sdi0 <- 0.1
# sdj0 <- 0.1

# Model name
model <- "test"

# Run simulations
source(file.path("scripts", "modelsetup.R"))
