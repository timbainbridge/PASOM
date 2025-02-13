############################ Single run focus ##################################
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
txt_scale <- 8

# Select simulations -----------------------------------------------------------
mean_op0 <- results$opinion$mean
rm(results)
mean_op <- mean_op0[nrow(mean_op0), ]
mixed_sel <- names(mean_op)[
  abs(mean_op - mean(mean_op)) == min(abs(mean_op - mean(mean_op)))
]
pros_sel <- names(mean_op)[mean_op == max(mean_op)]
anti_sel <- names(mean_op)[mean_op == min(mean_op)]

# Starting network -------------------------------------------------------------
g0 <- readRDS(file.path("output", model, "g0.rds"))
bs <- (stcon0 / (ths0)) * stcdv0^-pers0
as <- (stcon0 * (ths0)) * stcdv0^pers0
bs[bs < 1] <- 1
as[as < 1] <- 1
ps <- ((as - 1/3) / (as + bs - 2/3))
V(g0)$color <- ps
names(ps) <- V(g0)$name
op <- ifelse(ps >= .6, 1, ifelse(ps <= .4, -1, 0))
echo0 <- mapply(
  function(x, y) {
    # Proportion of connections with pro-science view
    m <- (ps[y] >= .5) |> mean()
    # In pro (1) or anti (-1) science echo chamber or neither (0)?
    ifelse(x == 1 & m >= .9, 1, ifelse(x == -1 & m <= .1, -1, 0))
  },
  x = op, y = adjacent_vertices(g0, V(g0)$name)
)

# Data for plots ---------------------------------------------------------------
begin <- list(g = g0, opinion1 = ps, echo = echo0)
tmp <- list(
  opinion1 = readRDS(file.path("output", model, "opinion1.rds"))[[mixed_sel]],
  echo = readRDS(file.path("output", model, "echo.rds"))[[mixed_sel]]
)
mixed <- list(
  g = readRDS(file.path("output", model, "g.rds"))[[mixed_sel]],
  opinion1 = tmp$opinion1[, ncol(tmp$opinion1)],
  echo = tmp$echo[, ncol(tmp$echo)]
)
tmp <- list(
  opinion1 = readRDS(file.path("output", model, "opinion1.rds"))[[pros_sel]],
  echo = readRDS(file.path("output", model, "echo.rds"))[[pros_sel]]
)
pros <- list(
  g = readRDS(file.path("output", model, "g.rds"))[[pros_sel]],
  opinion1 = tmp$opinion1[, ncol(tmp$opinion1)],
  echo = tmp$echo[, ncol(tmp$echo)]
)
tmp <- list(
  opinion1 = readRDS(file.path("output", model, "opinion1.rds"))[[anti_sel]],
  echo = readRDS(file.path("output", model, "echo.rds"))[[anti_sel]]
)
anti <- list(
  g = readRDS(file.path("output", model, "g.rds"))[[anti_sel]],
  opinion1 = tmp$opinion1[, ncol(tmp$opinion1)],
  echo = tmp$echo[, ncol(tmp$echo)]
)
rm(tmp)
V(mixed$g)$color <- mixed$opinion1
V(pros$g)$color <- pros$opinion1
V(anti$g)$color <- anti$opinion1

# Create figures ---------------------------------------------------------------
leg1 <- fig.fun(mixed$g, TRUE) |> get_legend2()
if (model == "base") {
  leg2 <- dens.fun2(mixed, TRUE) |> get_legend2()
} else {
  leg2 <- readRDS(file.path("plots", "legend.rds"))
}
fig_network <- plot_grid(
  leg1,
  plot_grid(
    fig.fun(g0), fig.fun(pros$g), fig.fun(anti$g), fig.fun(mixed$g),
    ncol = 1,
    labels = c("A", "C", "E", "G")
  ),
  plot_grid(
    dens.fun2(begin, txt_s = 4),
    dens.fun2(pros, txt_s = 4),
    dens.fun2(anti, txt_s = 4),
    dens.fun2(mixed, txt_s = 4),
    ncol = 1,
    labels = c("B", "D", "F", "H")
  ),
  leg2,
  ncol = 4,
  rel_widths = c(1, 6, 6, 2)
)
if (model == "bt_-0.5_stc_40") {
  fig_gab_red <- plot_grid(
    dens.fun2(pros, point = FALSE), dens.fun2(anti, point = FALSE),
    ncol = 2,
    labels = "AUTO"
  )
}
