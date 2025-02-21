################ A file to produce boxplots for the paper ######################
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Setup ------------------------------------------------------------------------
# Packages
pkgs <- list(
  "ggplot2"       # For all the figures
  , "reshape"     # For melt()
  , "paletteer"   # For palettes
  , "psych"       # For describe()
  , "cowplot"     # For plot_grid()
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Custom functions
source(file.path("scripts", "functions_fig.R"))

# Data -------------------------------------------------------------------------
esel <- c("echo", "echo_inc", "echo_ex")
edat0 <- lapply(
  setNames(nm = esel),
  function(x) readRDS(file.path("output", "base", paste0(x, ".rds")))
)
edat1 <- lapply(
  edat0,
  function(y) {
    sapply(
      y,
      function(z) {
        if (length(dim(z)) == 2) {
          (z[, 200] != 0) |> mean()
        } else {
          (z != 0) |> mean()
        }
      }
    )
  }
) |>
  as.data.frame()
edat1$hphil <- readRDS(file.path("results", "base_r.rds"))$homophily
edat <- melt(edat1)
levels(edat$variable) <- c("Standard", "Inclusive", "Exclusive", "Homophily")

# Figure -----------------------------------------------------------------------
eplot <-
  ggplot(edat, aes(x = variable, y = value, colour = )) +
  geom_boxplot(outlier.alpha = 0) +
  geom_point(position = position_jitter(width = .1, height = 0)) +
  scale_y_continuous(
    "Proportion of agents in an echo chamber / homophily",
    7:10*.1,
    15:19*.05,
    limits = c(0.7, 1),
    expand = c(0, 0.01)
  ) +
  # scale_colour_manual(
  #   values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 4), -1
  # ) +
  theme_bw() +
  labs(x = "Echo chamber definition")
eplot

# Mixed model selection --------------------------------------------------------
mean_op <- readRDS(file.path("results", "base_r.rds"))$opinion$mean[200, ]
mixed_sel <- names(mean_op)[abs(mean_op - .5) == min(abs(mean_op - .5))] |>
  sub(pattern = "iter", "", x = _) |>
  as.numeric()
mixed0 <- sapply(
  c("echo", "echo_ex", "g", "opinion1"),
  function(x) {
    readRDS(file.path("output", "base", paste0(x, ".rds")))[[mixed_sel]]
  }
)
mixed <- mixed0
mixed$opinion1 <- mixed0$opinion1[, 200]
V(mixed$g)$color <- mixed$opinion1
mixed$echo <- mixed0$echo[, 200]
mixed$echo[mixed$echo != mixed$echo_ex] <- 2
nbhop <- sapply(
  adjacent_vertices(mixed$g, V(mixed$g)$name),
  function(x) mixed$opinion1[V(mixed$g) %in% x] |> mean()
)
ddat <- data.frame(
  agent = mixed$opinion1,
  neighbours = nbhop,
  echo = factor(
    mixed$echo,
    levels = -1:2,
    labels = c("Anti-Science", "Neither", "Pro-Science", "Discrepant")
  )
)

# Figure
p <- ggplot(ddat, aes(x = agent, y = neighbours)) +
  geom_density2d_filled(adjust = .5, bins = 20) +
  scale_fill_manual(
    values = paletteer_c("grDevices::Lajolla", 20, -1),
    guide = "none"
  ) +
  new_scale_colour() +
  geom_point(
    aes(x = agent, y = neighbours, colour = echo),
    ddat,
    alpha = .8,
    size = ifelse(ddat$echo != 2, .4, 1)
  ) +
  scale_colour_manual(
    values = setNames(
      c("#80C673", "#F4F8FB", "#8DBBDC", "red"),
      c("Anti-Science", "Neither", "Pro-Science", "Discrepant")
    ),
    guide = guide_legend(
      override.aes = list(size=2), title = "Echo Chamber\nMembership"
    )
  ) +
  theme_bw() +
  scale_x_continuous(limits = 0:1, expand = rep(0, 4)) +
  scale_y_continuous(limits = 0:1, expand = rep(0, 4)) +
  theme(
    legend.position = c(1.2, 0.5),
    legend.key = element_rect(
      fill = paletteer_c("grDevices::Lajolla", 20, -1)[[2]]
    ),
    axis.title = element_text(size = 10),
    plot.margin = margin(t = 5, r = 40, b = 5, l = 5)
  ) +
  labs(x = "Agents' opinions", y = "Average of neighbours' opinions")
p_xm <- axis_canvas(p, axis = "x") +
  geom_histogram(
    data = ddat,
    mapping = aes(x = agent),
    colour = "black",
    fill = "gray",
    breaks = 0:40*.025
  )
p_ym <- axis_canvas(p, axis = "y") +
  geom_histogram(
    data = ddat,
    mapping = aes(y = neighbours),
    colour = "black",
    fill = "gray",
    breaks = 0:40*.025
  )
dens <- insert_xaxis_grob(p, p_xm, position = "top") |>
  insert_yaxis_grob(p_ym, position = "right")
ggdraw(dens)

# Save -------------------------------------------------------------------------
ggsave2(
  file.path("plots", "echo_plot.pdf"), eplot,
  height = 10, width = 8, units = "cm"
)
ggsave2(
  file.path("plots", "dens_echo_comp.pdf"), dens,
  height = 12, width = 12, units = "cm"
)
