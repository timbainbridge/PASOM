########################## Default starting objects ############################
#
# Loads the default starting objects.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

if (n_agent0 == 500) {
  pers0 <- readRDS(file.path("input", "pers00.rds"))
  g0 <- readRDS(file.path("input", "g0.rds"))
} else {
  if (n_agent0 == 1000) {
    pers0 <- readRDS(file.path("input", "pers20.rds"))
    g0 <- readRDS(file.path("input", "g1000.rds"))
  } else warning(paste("No priors or network for n_agent0 =", n_agent0))
}
