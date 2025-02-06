########################### Fixed Network Setup ################################
#
# Creates a default network and agents' starting priors.
# The rationale is that, in a real-world example, one takes a particular
# network as given, and does not take randomness in their generation into
# account (i.e., the network could've had different people in it or different
# connections between those people).
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Packages ---------------------------------------------------------------------
library(igraph)

# Create obejct directory (if it doesn't exist) --------------------------------
if (!dir.exists("input")) dir.create("input")

# Load parameters for network creation -----------------------------------------
source(file.path("scripts", "default_params.R"))
n_agent <- n_agent0

# Network parameters for N = 1000
n_agent2 <- 1000         # Number of agents
group_n2 <- n_agent2/5  # Number of groups
p_in <- 1               # P of connecting with someone in one's group
p_out2 <- 1/n_agent2    # P of connecting with someone not in one's group

# Network ----------------------------------------------------------------------

# Network creation objects
pf0 <- matrix(rep(p_out, times = group_n^2), nrow = group_n)
diag(pf0) <- p_in

# Network 1
set.seed(0)
g0 <- sample_pref(n_agent, group_n, pref.matrix = pf0)
V(g0)$name <- formatC(1:n_agent,
                      width = nchar(n_agent),
                      format = "d",
                      flag = "0")
# Connect unconnected segments to other segments
compon <- components(g0)
while (compon$no > 1) {
  # Sample misbehaves badly when sampling from a length 1 object
  # (it samples from 1:x, not just from x).
  if (length(compon$membership[compon$membership == 1]) == 1) {
    edgesel1 <- compon$membership[compon$membership == 1] |>
      names() |>
      as.numeric()
  } else {
    edgesel1 <- sample(compon$membership[compon$membership == 1], 1) |>
      names() |>
      as.numeric()
  }
  if (length(compon$membership[compon$membership == 2]) == 1) {
    edgesel2 <- compon$membership[compon$membership == 2] |>
      names() |>
      as.numeric()
  } else {
    edgesel2 <- sample(compon$membership[compon$membership == 2], 1) |>
      names() |>
      as.numeric()
  }
  g0 <- g0 + edge(c(edgesel1, edgesel2))
  compon <- components(g0)
}

# Network 2 --------------------------------------------------------------------
set.seed(1)
g0_1 <- sample_pref(n_agent, group_n, pref.matrix = pf0)
V(g0_1)$name <- formatC(1:n_agent,
                        width = nchar(n_agent),
                        format = "d",
                        flag = "0")
# Connect unconnected segments to other segments
compon <- components(g0_1)
while (compon$no > 1) {
  if (length(compon$membership[compon$membership == 1]) == 1) {
    edgesel1 <- compon$membership[compon$membership == 1] |>
      names() |>
      as.numeric()
  } else {
    edgesel1 <- sample(compon$membership[compon$membership == 1], 1) |>
      names() |>
      as.numeric()
  }
  if (length(compon$membership[compon$membership == 2]) == 1) {
    edgesel2 <- compon$membership[compon$membership == 2] |>
      names() |>
      as.numeric()
  } else {
    edgesel2 <- sample(compon$membership[compon$membership == 2], 1) |>
      names() |>
      as.numeric()
  }
  g0_1 <- g0_1 + edge(c(edgesel1, edgesel2))
  compon <- components(g0_1)
}

# Network 3 --------------------------------------------------------------------
set.seed(2)
g0_2 <- sample_pref(n_agent, group_n, pref.matrix = pf0)
V(g0_2)$name <- formatC(1:n_agent,
                        width = nchar(n_agent),
                        format = "d",
                        flag = "0")
# Connect unconnected segments to other segments
compon <- components(g0_2)
while (compon$no > 1) {
  if (length(compon$membership[compon$membership == 1]) == 1) {
    edgesel1 <- compon$membership[compon$membership == 1] |>
      names() |>
      as.numeric()
  } else {
    edgesel1 <- sample(compon$membership[compon$membership == 1], 1) |>
      names() |>
      as.numeric()
  }
  if (length(compon$membership[compon$membership == 2]) == 1) {
    edgesel2 <- compon$membership[compon$membership == 2] |>
      names() |>
      as.numeric()
  } else {
    edgesel2 <- sample(compon$membership[compon$membership == 2], 1) |>
      names() |>
      as.numeric()
  }
  g0_2 <- g0_2 + edge(c(edgesel1, edgesel2))
  compon <- components(g0_2)
}

# Grid Network -----------------------------------------------------------------
set.seed(0)
gg <- make_lattice(c(10, 10, 5))
V(gg)$name <- formatC(1:n_agent,
                      width = nchar(n_agent),
                      format = "d",
                      flag = "0")

# Network with N = 1000 --------------------------------------------------------

# Network creation objects
pf02 <- matrix(rep(p_out2, times = group_n2^2), nrow = group_n2)
diag(pf02) <- p_in

set.seed(0)
g1000 <- sample_pref(n_agent2, group_n2, pref.matrix = pf02)
V(g1000)$name <- formatC(1:n_agent2,
                      width = nchar(n_agent2),
                      format = "d",
                      flag = "0")
# Connect unconnected segments to other segments
compon <- components(g1000)
while (compon$no > 1) {
  # Sample misbehaves badly when sampling from a length 1 object
  # (it samples from 1:x, not just from x).
  if (length(compon$membership[compon$membership == 1]) == 1) {
    edgesel1 <- compon$membership[compon$membership == 1] |>
      names() |>
      as.numeric()
  } else {
    edgesel1 <- sample(compon$membership[compon$membership == 1], 1) |>
      names() |>
      as.numeric()
  }
  if (length(compon$membership[compon$membership == 2]) == 1) {
    edgesel2 <- compon$membership[compon$membership == 2] |>
      names() |>
      as.numeric()
  } else {
    edgesel2 <- sample(compon$membership[compon$membership == 2], 1) |>
      names() |>
      as.numeric()
  }
  g1000 <- g1000 + edge(c(edgesel1, edgesel2))
  compon <- components(g1000)
}

# Values for creating priors ---------------------------------------------------
set.seed(0)

# No need for values to be random, only order. Take evenly from (0:1) range.

# N = n_agent (500)
pers00 <- qnorm(1:n_agent/(n_agent + 1), 0, 1 / qnorm(.625))
pers00 <- sample(pers00, length(pers00))
# pers00 <- rnorm(n_agent, 0, 1 / qnorm(.625))  # ~75% have mag > 1

# We need a funky distribution to generate more values close to 0 and more
# values further from 1. Values close to 0 create agents who are undecided,
# values further from 1 create agents with stronger starting opinions.
# Squaring and giving the correct direction achieves this.
pers01 <- sign(pers00) * (pers00^2)

# N = 2 x n_agent (1000)
pers20 <- qnorm(1:n_agent2/(n_agent2 + 1), 0, 1 / qnorm(.625))
pers20 <- sample(pers20, length(pers20))
# pers10 <- rnorm(n_agent, 0, 1 / qnorm(.625))
pers21 <- sign(pers20) * (pers20^2)
# pers20 <- c(pers00, pers10)
# pers21 <- c(pers01, pers11)

# Save -------------------------------------------------------------------------

# Just in case set.seed has not worked as expected, don't overwrite objects
# unless necessary.

# saveRDS(g0, file.path("input", "g0.rds"))
# saveRDS(g0_1, file.path("input", "g0_1.rds"))
# saveRDS(g0_2, file.path("input", "g0_2.rds"))
# saveRDS(gg, file.path("input", "gg.rds"))
# saveRDS(g1000, file.path("input", "g1000.rds"))
# saveRDS(persi, file.path("input", "persi.rds"))
# saveRDS(persi2, file.path("input", "persi2.rds"))
saveRDS(pers00, file.path("input", "pers00.rds"))
saveRDS(pers01, file.path("input", "pers01.rds"))
saveRDS(pers20, file.path("input", "pers20.rds"))
saveRDS(pers21, file.path("input", "pers21.rds"))
