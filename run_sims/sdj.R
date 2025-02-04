# Code to run the model

# Baseline parameter values
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
sdj0 <- sqrt(sdi0^2 + sdj0^2)
sdi0 <- 0

# Object changes from defaults for the current simulations
# None

# Model name
model <- "sdj"

# Run simulations
source(file.path("scripts", "modelsetup.R"))
