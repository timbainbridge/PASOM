# Code to run the model

# Baseline parameter values
source(file.path("scripts", "default_params.R"))
n_agent0 <- 1000  # The below script accounts for N (for N = 500 or 1000).
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
# No changes from default, only the graphs changes.

# Object changes from defaults for the current simulations
# Objects are unique but change in source above for n_agent0 == 1000.

# Model name
model <- "g1000"

# Run simulations
source(file.path("scripts", "modelsetup.R"))
