########################## Median sim density ##################################
#
# Some results from selected simulations. Produces Figures 2 and 5 from the
# paper.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Setup ------------------------------------------------------------------------

# Load packages
pkgs <- list(
  "igraph"       # For networks
  , "ggplot2"    # For all the figures
  , "paletteer"  # For colour palettes
  , "ggnewscale"  # For different colours in density plots.
  , "cowplot"     # For plot_grid()  (4th time lucky? Yes!)
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Create object directory (if it doesn't exist)
if (!dir.exists("plots")) dir.create("plots")

source(file.path("scripts", "functions_fig.R"))

# Load results -----------------------------------------------------------------
results <- readRDS(file.path("results", paste0(model, "_r.rds")))

# Select simulations -----------------------------------------------------------
mean_op0 <- results$opinion$mean
rm(results)
mean_op <- mean_op0[nrow(mean_op0), ]
med_sel <- names(mean_op)[mean_op == sort(mean_op)[length(mean_op)/2 + 1]]
tmp <- list(
  opinion1 = readRDS(file.path("output", model, "opinion1.rds"))[[med_sel]],
  echo = readRDS(file.path("output", model, "echo.rds"))[[med_sel]]
)
med <- list(
  g = readRDS(file.path("output", model, "g.rds"))[[med_sel]],
  opinion1 = tmp$opinion1[, ncol(tmp$opinion1)],
  echo = tmp$echo[, ncol(tmp$echo)]
)
rm(tmp)
V(med$g)$color <- med$opinion1

# Create figures ---------------------------------------------------------------
fig_med <- dens.fun2(med, point = FALSE, labs0 = FALSE)
