# Code to run the model

# Baseline parameter values and objects
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
btp0 <- 0

# Object changes from defaults for the current simulations
# None for this sim

# Input data
model <- "btp0"

# Run simulations
source(file.path("scripts", "modelsetup.R"))
