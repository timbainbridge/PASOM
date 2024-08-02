############################ Single run focus ##################################
#
# Produces results from single simulations for the supplement.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Setup ------------------------------------------------------------------------

# Load packages
pkgs <- list(
  "igraph"        # For networks
  , "ggplot2"     # For all the figures
  , "paletteer"   # For the palettes
  , "ggpubr"      # For ggarrange()
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Create object directory (if it doesn't exist
if (!dir.exists("plots")) dir.create("plots")

# Load objects -----------------------------------------------------------------

# Model outputs
models_sup <- c(
  "base_np"
  , "g1_np"
  , "g2_np"
  , "lattice_np"
  , "g1000_np"
  , "stcon_high_np"
  , "bt1_np"
  , "sd5_np"
  , "sdswap_np"
  , "ck8_np"
  , "cb1_np"
  , "d0_np"
)
model_names <- c("Base", "C1", "C2", "L", "A1000", "HSP", "BT1", "SD.5",
                 "SDSwap", "CK.08", "CB.1", "D0")

# Default parameters (for rounds0)
source(file.path("scripts", "default_params.R"))

rds <- rounds0
mean_op <- sapply(
  models_sup,
  function(x) {
    tmp <- readRDS(file.path("results", paste0(x, "_r.rds")))
    tmp[["opinion"]][["mean"]][rds, ]
  }
)
mixed_sel <- apply(
  mean_op,
  2,
  function(x) {
    names(x)[abs(x - .5) == min(abs(x - .5))] |>
      sub(pattern = "iter", "", x = _) |>
      as.numeric()
  }
)
obj_sel <- c("g", "opinion1")
mixed <- mapply(
  function(x, y) {
    tmp <- readRDS(file.path("output", paste0(x, ".rds")))[[y]][obj_sel]
    tmp1 <- tmp$opinion1[, rds]
    V(tmp$g)$color <- tmp1
    return(tmp$g)
  },
  x = models_sup, y = mixed_sel, SIMPLIFY = FALSE
)

# Function to create figures ---------------------------------------------------
fig.fun <- function(g, i) {
  set.seed(0)  # For layout_nicely()
  g_coord <- data.frame(agent = V(g)$name,
                        layout_nicely(g),
                        Opinion = V(g)$color)
  g_edges <- as_data_frame(g)
  g_edges$from.x1 <- g_coord$X1[match(g_edges$from, g_coord$agent)]
  g_edges$from.x2 <- g_coord$X2[match(g_edges$from, g_coord$agent)]
  g_edges$to.x1 <- g_coord$X1[match(g_edges$to, g_coord$agent)]
  g_edges$to.x2 <- g_coord$X2[match(g_edges$to, g_coord$agent)]
  gg_g <-
    ggplot() +
    geom_segment(aes(x = from.x1, xend = to.x1, y = from.x2, yend = to.x2),
                 g_edges,
                 colour="black",
                 alpha = .2,
                 linewidth = .4) +
    geom_point(aes(x = X1, y = X2), g_coord, size = 1.5, colour = "black") +
    geom_point(aes(x = X1, y = X2, colour = Opinion), g_coord) +
    scale_colour_gradientn(
      colours = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 101, -1),
      limits = 0:1,
      breaks = 0:2*.5,
      labels = 0:2*.5
    ) +
    theme_void() +
    ggtitle(paste0(i))
}
hist.fun <- function(p, i) {
  gg_hist <-
    ggplot(data.frame(ps = p), aes(x = ps)) +
    geom_histogram(
      breaks = 0:50*.02, alpha = .8,
      fill = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 50, -1),
      colour = "black"
    ) +
    xlab("Opinion") +
    ylab("Count") +
    xlim(0:1) +
    theme_bw() +
    ggtitle(paste0(i))
}

# Create figures ---------------------------------------------------------------
fig_compare_n0 <- mapply(function(x, y) fig.fun(x, y),
                         x = mixed,
                         y = model_names,
                         SIMPLIFY = FALSE)
fig_compare_h0 <- mapply(function(x, y) hist.fun(V(x)$color, y),
                       x = mixed,
                       y = model_names,
                       SIMPLIFY = FALSE)
fig_compare_n <- ggarrange(plotlist = fig_compare_n0,
                           ncol = 2,
                           nrow = length(fig_compare_n0)/2,
                           common.legend = TRUE,
                           legend = "left")
fig_compare_h <- ggarrange(plotlist = fig_compare_h0,
                           ncol = 2,
                           nrow = length(fig_compare_h0)/2,
                           common.legend = TRUE,
                           legend = "left")

# Save figures -----------------------------------------------------------------
saveRDS(fig_compare_n, file.path("plots", "sup_compare_n.rds"))
saveRDS(fig_compare_h, file.path("plots", "sup_compare_h.rds"))
