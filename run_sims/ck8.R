# Code to run the model

# Baseline parameter values
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
ckc <- c(.8, 1.25)
ck0_d <- ck0

# Object changes from defaults for the current simulations
# None

# Run simulations
for (m in ckc) {
  ck0 <- ck0_d * m
  
  # Model name
  model <- paste0("ck", m)
  message(model)
  
  # Run
  source(file.path("scripts", "modelsetup.R"))
}
