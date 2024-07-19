# Code to run the model

# Baseline parameter values
source(file.path("scripts", "default_params.R"))

# Changes for current simulations
# No changes from default, only the graphs changes.

# Input data
pers_input <- readRDS(file.path("input", "persi.rds"))
graph_input <- readRDS(file.path("input", "gg.rds"))  # Grid / lattice network
model_name <- "lattice_np"

# Run simulations
source(file.path("scripts", "modelsetup_np.R"))