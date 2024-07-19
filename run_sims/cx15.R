# Code to run the model

# Baseline parameter values
source(file.path("scripts", "default_params.R"))

# Changes for current simulations
cx0 <- 1.5

# Input data
pers_input <- readRDS(file.path("input", "persi.rds"))
graph_input <- readRDS(file.path("input", "g0.rds"))
model_name <- "cx15_np"

# Run simulations
source(file.path("scripts", "modelsetup_np.R"))