# Code to run the model

# Baseline parameter values and objects
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
dc <- c(.5, 0)
dt0_d <- dt0
da0_d <- da0

# Object changes from defaults for the current simulations
# None

# Run simulations
for (m in dc) {
  dt0 <- dt0_d * m  # Completely myopic at dc = 0.
  da0 <- da0_d * m
  
  # Model name
  model <- paste0("d", m)
  message(model)
  
  # Run
  source(file.path("scripts", "modelsetup.R"))
}
