# Code to run the model

# Baseline parameter values and objects
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
parset <- list(bt = c(.8, .4, .2, .04, 0), gma = c(.2, .5, 1, 2))

# Object changes from defaults for the current simulations
# None for this sim

# Run simulations
for (m in parset$bt) {
  btp0 <- btp0 * m
  btg0 <- btg0 * m
  for (n in parset$gma) {
    gma0 <- n
    
    # Model name
    model <- paste0("bt", m, "_gma", n)
    
    source(file.path("scripts", "modelsetup.R"))
  }
}
