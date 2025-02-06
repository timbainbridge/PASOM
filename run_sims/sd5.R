# Code to run the model

# Baseline parameter values
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
sdc <- c(.5, 2)
sdi0_d <- sdi0
sdj0_d <- sdj0

# Object changes from defaults for the current simulations
# None

# Run simulations
for (m in sdc) {
  sdi0 <- sdi0_d * m
  sdj0 <- sdj0_d * m
  
  # Model name
  model <- paste0("sd", m)
  message(model)
  
  # Run
  source(file.path("scripts", "modelsetup.R"))
}
