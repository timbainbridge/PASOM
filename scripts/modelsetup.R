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
  , "parallel"    # For mclapply()
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Model functions
source(file.path("scripts", "functions.R"))

# Output save location 
if (!dir.exists("output")) dir.create("output")

# Checks -----------------------------------------------------------------------

# All parameters
params <- c(
  iter = iter0, rounds = rounds0, stcon = stcon0, stcdv = stcdv0,
  shrpp = shrpp0, aa = aa0, at = at0, dt = dt0, da = da0, sdi = sdi0,
  sdj = sdj0, bap0 = bap0, btp = btp0, ccp = ccp0, bag = bag0, btg = btg0,
  ck = ck0, cb = cb0, ths = ths0, u = u0, cx = cx0, nu = nu0, mu = mu0,
  p1 = p10, gma = gma0
)

# Hash of functions file
functions1 <- readLines(file.path("scripts", "functions.R")) |>
  paste0(collapse = "\n")

# Parameters from a prior run?
if (file.exists(file.path("output", paste0("params_", model, ".rds")))) {
  params_in <-
    readRDS(file.path("output", paste0("params_", model, ".rds")))
} else {
  params_in <- rep(0, length(params))
}

# Functions from a prior run?
if (file.exists(file.path("output", "functions.rds"))) {
  functions0 <- readRDS(file.path("output", "functions.rds"))
} else {
  functions0 <- 0
}

# Run?
if ((list.files(file.path("output", model)) |> length()) == 0) {
  run0 <- TRUE
} else {
  run0 <- FALSE
  if (length(params) != length(params_in)) {
    print(paste(
      "Major model changes. The number of parameters has changed.",
      "Move or delete the output (or rename the folder) and run again."
    ))
  } else {
    if (sum(params != params_in) != 0) {
      print(paste(
        "Different parameter values: Rename/delete the output and run again."
      ))
    } else {
      if (!functions0 == functions1) {
        print(paste(
          "Functions' hashes do not match. If the changes were superficial,",
          "update the hash. If they were not (or you do not know), rename the",
          "output for ALL models and run them ALL again."
        ))
      } else {
        print(paste(
          "Looks as though everything is the same. Unless something",
          "is amiss, there is no reason to re-run this model."
        ))
      }
    }
  }
}

# Run model --------------------------------------------------------------------
if (run0) {
  n_agent0 <- length(g0)
  # Can set mc_cores in run sim file to specify the number to use.
  # Otherwise 8 will be used (may not work on Windows)
  if (!exists("mc_cores")) {
    mc_cores <- 8
  }
  output <- mclapply(
    1:iter0,
    function(j) {
      # Using `set.seed(j)` may have caused results to be correlated by
      # iteration as each draw for e_j and f_ij should be identical.
      # Setting the seed based on iteration and parameter values instead means
      # different seeds are created for each parameter set so results will not
      # be correlated by iteration.
      
      # To avoid complication, we have selected enough simulations for
      # comparisons to be made without worrying about identical random draws by
      # parameter sets.
      set.seed(j + round(sum(params) * 100, 0))
      print(j)
      fun.b(
        g0 = g0, pers0 = pers0, iter = iter0, n_agent = n_agent0,
        rounds = rounds0, stcon = stcon0, stcdv = stcdv0, shrpp = shrpp0,
        dt1 = dt0, da1 = da0, sdi1 = sdi0, sdj1 = sdj0,
        bap1 = bap0, btp1 = btp0, ccp1 = ccp0,
        bag1 = bag0, btg1 = btg0, ck1 = ck0, cb1 = cb0,
        ths1 = ths0, u1 = u0, cx1 = cx0, nu1 = nu0, mu1 = mu0,
        p11 = p10, gma = gma0, at = at0, aa = aa0
      )
    },
    mc.cores = mc_cores, mc.set.seed = FALSE
  )
  names(output) <- paste0("iter", seq_along(output))

  # Save outputs ---------------------------------------------------------------
  if (!dir.exists(file.path("output", model))) {
    dir.create(file.path("output", model))
  }
  # Save by category not iteration. 
  for (i in names(output[[1]])) {
    if (i == "g0") {
      # g0 is the same for all sims. No need to save it more than once.
      saveRDS(output[[1]][[i]], file.path("output", model, "g0.rds"))
    } else {
      tmp <- lapply(output, function(x) x[[i]])
      saveRDS(tmp, file.path("output", model, paste0(i, ".rds")))
    }
  }
  saveRDS(params, file.path("output", paste0("params_", model, ".rds")))
  saveRDS(functions1, file.path("output", "functions.rds"))
}
# Produce results if outputs exist (i.e., if it just ran or ran previously)
if (!file.exists(file.path("output", paste0("params_", model, ".rds")))) {
  
  # Produce aggregate data -----------------------------------------------------
  print("Aggregated output")
  source(file.path("scripts", "output_s.R"))
  saveRDS(results, file.path("results", paste0(model, "_r.rds")))
  rm(results)
  
  # Produce model figures ------------------------------------------------------
  print("Time series plot")
  source(file.path("scripts", "results_s.R"))
  saveRDS(plots, file.path("plots", paste0(model, "_p.rds")))
  ggsave(
    file.path("plots", paste0(model, ".png")),
    plots,
    width = 3000, height = 3000, units = "px"
  )
  if (model == "base") {
    ggsave(
      file.path("plots", "Fig1.eps"),
      plots,
      width = 6, height = 6, dpi = 450
    )
  }
  print("Network and neigbours plot")
  source(file.path("scripts", "results_s2.R"))
  # Save figures
  ggsave(
    file.path("plots", paste0(model, "_network.png")),
    plot = fig_network,
    width = 3000, height = 3500, units = "px"
  )
  if (model == "base") {
    ggsave(
      file.path("plots", "Fig2.eps"), plot = fig_network,
      width = 6, height = 7, dpi = 450
    )
    saveRDS(leg2, file.path("plots", "legend.rds"))
  }
  if (model == "bt_-0.5_stc_40") {
    ggsave2(
      file.path("plots", "gab_red.png"), plot = fig_gab_red,
      width = 6, height = 3, dpi = 450
    )
    ggsave2(
      file.path("plots", "Fig8.esp"), plot = fig_gab_red,
      width = 6, height = 3, dpi = 450
    )
  }
  print("Median neighbour plot")
  source(file.path("scripts", "results_s3.R"))
  saveRDS(fig_med, file.path("plots", paste0(model, "_med.rds")))
}
