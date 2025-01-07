# Code to run the model

# Baseline parameter values and objects
source(file.path("scripts", "default_params.R"))
source(file.path("scripts", "default_objects.R"))

# Parameter changes from defaults for the current simulations
gma0 <- 2  # Allow random connections to be with anyone
cx <- c(1, .5, .1, 0)
stc <- c(1, 5, 40)
mu_0 <- c(500, 750, 2000)
stcon0_d <- stcon0

# Object changes from defaults for the current simulations
# None for this sim

# Run simulations
for (m in cx) {
  cx0 <- m
  
  for (n in seq_along(stc)) {
    stcon0 <- stcon0_d * stc[n]
    mu0 <- mu_0[n]
    
    # Model name
    model <- paste0("cx_", m, "_stc_", stc[n])
    print(model)
    
    # Run
    source(file.path("scripts", "modelsetup.R"))
  }
}
