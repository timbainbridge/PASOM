# Code to run the model

# Baseline parameter values and objects
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
uc <- c(0, .4, .8)

# Object changes from defaults for the current simulations
# None

# Run simulations
for (m in uc) {
  u0 <- m
  
  # Model name
  model <- paste0("u", m)
  message(model)
  
  # Run
  source(file.path("scripts", "modelsetup.R"))
}
