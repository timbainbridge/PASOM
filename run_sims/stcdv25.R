# Code to run the model

# Baseline parameter values and objects
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
stcdv0 <- 2.5

# Object changes from defaults for the current simulations
# None for this sim

# Input data
model <- "stcdv25"

# Run simulations
source(file.path("scripts", "modelsetup.R"))
