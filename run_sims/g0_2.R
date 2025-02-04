# Code to run the model

# Baseline parameter values
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
# No changes from default, only the graphs changes.

# Object changes from defaults for the current simulations
g0 <- readRDS(file.path("input", "g0_2.rds"))
model <- "g0_2"

# Run simulations
source(file.path("scripts", "modelsetup.R"))
