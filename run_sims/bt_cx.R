# Code to run the model

# Baseline parameter values and objects
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
gma0 <- 2  # Allow random connections to be with anyone
cx <- c(.5, .1, 0)
bt <- c(.5, .1, 0, -.1, -.25, -.5)
mu0 <- 2500
stcon0 <- 40 * 25

# Object changes from defaults for the current simulations
# None for this sim

# Run simulations
for (m in bt) {
  btp0 <- m
  btg0 <- .8 * m
  
  for (n in cx) {
    cx0 <- n
    
    # Model name
    model <- paste0("bt_", m, "_cx_", n)
    message(model)
    
    # Run
    source(file.path("scripts", "modelsetup.R"))
  }
}
