# Code to run the model

# Baseline parameter values and objects
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
rounds0 <- 75
btp0 <- 0
btg0 <- 0
stcon0 <- 1000
mu0 <- 3000

# Object changes from defaults for the current simulations


# Model name
model <- "twitter"

# Run simulations
source(file.path("scripts", "modelsetup.R"))
