# Code to run the model

# Baseline parameter values and objects
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
stcdv0 <- 1.05

# Object changes from defaults for the current simulations
pers0 <- readRDS(file.path("input", "pers01.rds"))

# Input data
model <- "stpriors"

# Run simulations
source(file.path("scripts", "modelsetup.R"))
