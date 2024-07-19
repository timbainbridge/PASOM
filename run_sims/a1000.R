# Code to run the model

# Baseline parameter values
source(file.path("scripts", "default_params.R"))

# Changes for current simulations
# No parameter changes from default

# Input data
pers_input <- readRDS(file.path("input", "persi2.rds"))
graph_input <- readRDS(file.path("input", "g1000.rds"))
model_name <- "g1000_np"

# Run simulations
source(file.path("scripts", "modelsetup_np.R"))