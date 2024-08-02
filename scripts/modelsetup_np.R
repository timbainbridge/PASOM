############################## Run model #######################################
#
# This script sets up and runs the simulation, taking input from a file in the
# `run_sims` folder.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Setup ------------------------------------------------------------------------

# Load packages
pkgs <- list(
  "igraph"        # For networks
  , "fastmatch"   # For fmatch() and %fin%
  , "openssl"     # For creating hashes
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Model functions
source(file.path("scripts", "functions2_np.R"))

# Output save location 
if (!dir.exists("output")) dir.create("output")

# Checks -----------------------------------------------------------------------

# All parameters
params <- c(
  iter0, rounds0, stcon0, shrpp0, aa0, at0, dt0, da0, sdi0, sdj0, bap0, btp0,
  ccp0, bsp0, bag0, btg0, ck0, cb0, ths0, cx0, nu0, mu0, p10, gma0
)

# Hash of functions file
hash_functions <- md5(
  readLines(file.path("scripts", "functions.R")) |> paste0(collapse = "\n")
)

# Parameters from a prior run?
if (file.exists(file.path("output", "params_", model_name, ".rds"))) {
  params_in <- readRDS(file.path("output", "params_", model_name, ".rds"))
} else {
  params_in <- rep(0, length(params))
}

# Functions from a prior run?
if (file.exists(file.path("output", "hash_functions.rds"))) {
  hash_functions0 <- readRDS(file.path("output", "hash_functions.rds"))
} else {
  hash_functions0 <- 0
}

# Run?
if (!file.exists(file.path("output", paste0(model_name, ".rds")))) {
  run0 <- TRUE
} else {
  run0 <- FALSE
  if (length(params != length(params_in))) {
    print(paste("Major model changes. The number of parameters has changed.",
                "Rename/delete the output and run again."))
  } else {
    if (sum(params != params_in) != 0) {
      print(paste0("Different parameter values: Rename/delete the output and",
                   "run again."))
    } else {
      if (!hash_functions0 == hash_functions) {
        print(paste(
          "Functions' hashes do not match. If the changes were superficial,",
          "update the hash. If they were not (or you do not know), rename the",
          "output for ALL models and run them ALL again."
        ))
      } else {
        print(paste("Looks as though everything is the same. Unless something",
                    "is amiss, there is no reason to re-run this model."))
      }
    }
  }
}

# Run model --------------------------------------------------------------------
if (run0) {
  set.seed(0)
  gf <- graph_input
  persf <- pers_input
  n_agent0 <- length(gf)
  output <- list()
  print(0)
  for (j in 1:iter0) {
    print(j)
    output[[j]] <- fun.b2(
      g0 = gf, pers0 = persf, iter = iter0, n_agent = n_agent0,
      rounds = rounds0, stcon = stcon0, shrpp = shrpp0,
      dt1 = dt0, da1 = da0, sdi1 = sdi0, sdj1 = sdj0,
      bap1 = bap0, btp1 = btp0, ccp1 = ccp0, bsp1 = bsp0,
      bag1 = bag0, btg1 = btg0, ck1 = ck0, cb1 = cb0,
      bs1 = bs0, cx1 = cx0, nu1 = nu0, mu1 = mu0, p11 = p10,
      gma = gma0, at = at0, aa = aa0
    )
  }
  names(output) <- paste0("iter", seq_along(output))

  # Save outputs ---------------------------------------------------------------
  saveRDS(output, file.path("output", paste0(model_name, ".rds")))
  saveRDS(params, file.path("output", paste0("params_", model_name, ".rds")))
  saveRDS(hash_functions, file.path("output", "hash_functions2.rds"))
}
