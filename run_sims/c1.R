# Code to run the model

# Baseline parameter values
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Changes for current simulations
# No changes from default, only the graphs changes.

# Input data
g0 <- g0 <- readRDS(file.path("input", "g0_1.rds"))
model_name <- "g0_1"

# Run simulations
source(file.path("scripts", "modelsetup.R"))
