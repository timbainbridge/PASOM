# Code to run the model

# Baseline parameter values
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
cbc <- c(.5, 2)
cb0_d <- cb0

# Object changes from defaults for the current simulations
# None

# Run simulations
for (m in cbc) {
  cb0 <- cb0_d * m
  
  # Model name
  model <- paste0("cb", m)
  message(model)
  
  # Run
  source(file.path("scripts", "modelsetup.R"))
}
