# Code to run the model

# Baseline parameter values and objects
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
gma0 <- 2  # Allow random connections to be with anyone
bt <- c(1, .5, .1, .05, 0, -.05, -.5)
btp0_d <- btp0
btg0_d <- btg0

# Object changes from defaults for the current simulations
# None for this sim

# Run simulations
for (m in bt) {
  btp0 <- btp0_d * m
  btg0 <- btg0_d * m
  
  # Model name
  model <- paste0("bt_", m)
  
  # Run
  source(file.path("scripts", "modelsetup.R"))
}
