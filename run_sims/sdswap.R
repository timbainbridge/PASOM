# Code to run the model

# Baseline parameter values
source(file.path("scripts", "default_params.R"))

# Baseline parameter values
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
sdi0_d <- sdi0
sdi0 <- sdj0
sdj0 <- sdi0_d

# Object changes from defaults for the current simulations
# None

# Model name
model <- "sdswap"

# Run simulations
source(file.path("scripts", "modelsetup.R"))
