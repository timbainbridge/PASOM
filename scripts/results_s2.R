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
  # , "ggpubr"     # For ggarrange()  # Does not work with ggMarginal()
  , "ggExtra"    # For ggMarginal()
  # , "patchwork"  # Alternative for ggpubr  # Bug in guides = "collect"
  , "gridExtra"  # Another alternative to ggarrange()
  , "ggnewscale"  # For different colours in density plots.
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Create object directory (if it doesn't exist)
if (!dir.exists("plots")) dir.create("plots")

# Function to create figures ---------------------------------------------------
fig.fun <- function(g, leg = FALSE) {
  set.seed(0)  # For layout_nicely()
  g_coord <- data.frame(agent = V(g)$name,
                        layout_nicely(g),
                        Opinion = V(g)$color)
  g_edges <- as_data_frame(g)
  g_edges$from.x1 <- g_coord$X1[match(g_edges$from, g_coord$agent)]
  g_edges$from.x2 <- g_coord$X2[match(g_edges$from, g_coord$agent)]
  g_edges$to.x1 <- g_coord$X1[match(g_edges$to, g_coord$agent)]
  g_edges$to.x2 <- g_coord$X2[match(g_edges$to, g_coord$agent)]
  ggplot() +
    geom_segment(aes(x = from.x1, xend = to.x1, y = from.x2, yend = to.x2),
                 g_edges,
                 colour="darkgray",
                 linewidth = .2) +
    geom_point(aes(x = X1, y = X2), g_coord, size = 1.5, colour = "black") +
    geom_point(aes(x = X1, y = X2, colour = Opinion), g_coord) +
    scale_colour_gradientn(
      colours = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 101, -1),
      limits = 0:1,
      breaks = 0:2*.5,
      labels = 0:2*.5
    ) +
    theme_void() +
    theme(legend.position = ifelse(leg, "left", "none"))
  # +
  #   ggtitle(paste0("(", i, ")"))
}
hist.fun <- function(p) {
  ggplot(data.frame(ps = p), aes(x = ps)) +
    geom_histogram(
      breaks = 0:50*.02,
      fill = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 50, -1),
      colour = "black"
    ) +
    xlab("Opinion") +
    ylab("Count") +
    xlim(0:1) +
    theme_bw()
  # +
  #   ggtitle(paste0("(", i, ")"))
}
dens.fun <- function(i, leg = FALSE) {
  tmp0 <- sapply(
    V(i$g)$name,
    function(x) {
      tmp1 <- adjacent_vertices(i$g, x)[[1]]
      # tmp1 <-
      i$opinion1[V(i$g) %in% tmp1] |> mean()
      # c(Agent = V(g)$color[V(g)$name == x], Neighbours = tmp1)
    }
  ) #|> t() |> as.data.frame()
  tmp <- data.frame(
    agent = i$opinion1[, ncol(i$opinion1)],
    neighbours = tmp0,
    echo = factor(
      i$echo[, ncol(i$echo)],
      levels = -1:1,
      labels = c("Anti-Science", "Neither", "Pro-Science")
    )
  )
  p0 <- ggplot(tmp, aes(x = agent, y = neighbours)) +
    geom_density2d_filled(adjust = .5, bins = 20) +
    scale_fill_manual(
      values = paletteer_c("grDevices::Lajolla", 20, -1),
      guide = "none"
    ) +
    new_scale_colour() +
    geom_point(
      aes(x = agent, y = neighbours, colour = echo),
      tmp,
      # colour = paletteer_c("grDevices::Lajolla", 10, -1)[8],
      size = .25
    ) +
    scale_colour_manual(
      values = setNames(
        paletteer_c("ggthemes::Green-Blue Diverging", 5)[2:4],
        c("Anti-Science", "Neither", "Pro-Science")
      ),
      guide = guide_legend(
        override.aes = list(size=2),
        title = "Echo Chamber\nMembership"
      )
    ) +
    theme_bw() +
    # theme(panel.border = unit(c(0, 0, 0, 0), "points")) +
    xlab("Agents' opinions") +
    ylab("Average of neighbours' opinions") +
    scale_x_continuous(limits = 0:1, expand = rep(0, 4)) +
    scale_y_continuous(limits = 0:1, expand = rep(0, 4)) +
    theme(
      legend.position = ifelse(leg, "right", "none"),
      legend.key = element_rect(
        fill = paletteer_c("grDevices::Lajolla", 20, -1)[[2]]
      )
    )
  p <- ggMarginal(p0, type = "densigram")
  return(p)
}

# Select simulations -----------------------------------------------------------
# mean_op0 <- readRDS(file.path("results", paste0(model, "_r.rds")))$opinion$mean
mean_op0 <- results$opinion$mean
mean_op <- mean_op0[nrow(mean_op0), ]
mixed_sel <- names(mean_op)[abs(mean_op - .5) == min(abs(mean_op - .5))]
pros_sel <- names(mean_op)[mean_op == max(mean_op)]
anti_sel <- names(mean_op)[mean_op == min(mean_op)]

# Only produce g0 for the base model
if (model == "base") {
  g0 <- readRDS(file.path("output", "base", "g0.rds"))[[1]]
  bs <- (stcon0 / (ths0)) * stcdv0^-pers0$i
  as <- (stcon0 * (ths0)) * stcdv0^pers0$i
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
}
mixed <- list(
  g = readRDS(file.path("output", model, "g.rds"))[[mixed_sel]],
  opinion1 = readRDS(file.path("output", model, "opinion1.rds"))[[mixed_sel]],
  echo = readRDS(file.path("output", model, "echo.rds"))[[mixed_sel]]
)
pros <- list(
  g = readRDS(file.path("output", model, "g.rds"))[[pros_sel]],
  opinion1 = readRDS(file.path("output", model, "opinion1.rds"))[[pros_sel]],
  echo = readRDS(file.path("output", model, "echo.rds"))[[pros_sel]]
)
anti <- list(
  g = readRDS(file.path("output", model, "g.rds"))[[anti_sel]],
  opinion1 = readRDS(file.path("output", model, "opinion1.rds"))[[anti_sel]],
  echo = readRDS(file.path("output", model, "echo.rds"))[[anti_sel]]
)
V(mixed$g)$color <- mixed$opinion1[, ncol(mixed$opinion1)]
V(pros$g)$color <- pros$opinion1[, ncol(pros$opinion1)]
V(anti$g)$color <- anti$opinion1[, ncol(anti$opinion1)]

# Create figures ---------------------------------------------------------------
leg1 <- cowplot::get_legend(fig.fun(pros$g, TRUE))
leg2 <- cowplot::get_legend(dens.fun(mixed, TRUE))
if (model == "base") {
  fig_network <- grid.arrange(
    fig.fun(g0), dens.fun(g0),
    fig.fun(pros$g), dens.fun(pros),
    fig.fun(anti$g), dens.fun(anti),
    fig.fun(mixed$g), dens.fun(mixed),
    leg1, leg2,
    widths = c(1, 5, 5, 1),
    layout_matrix = rbind(
      c(NA, 1, 2, NA), c(9, 3, 4, 10), c(9, 5, 6, 10), c(NA, 7, 8, 10)
    )
  )
} else {
  fig_network <- grid.arrange(
    fig.fun(pros$g), dens.fun(pros),
    fig.fun(anti$g), dens.fun(anti),
    fig.fun(mixed$g), dens.fun(mixed),
    leg1, leg2,
    widths = c(1, 5, 5, 1),
    layout_matrix = rbind(c(NA, 1, 2, NA), c(7, 3, 4, 8), c(NA, 5, 6, NA))
  )
}
fig_mixed <- list(network = fig.fun(mixed$g), density = dens.fun(mixed$g))

# Remove unneeded large objects (slows things when in memory)
rm(anti, pros, mixed)

# Save figures -----------------------------------------------------------------
ggsave(
  file.path("plots", paste0(model, "_network.png")),
  plot = fig_network,
  width = 2000, height = 3500, units = "px"
)
saveRDS(fig_mixed, file.path("plots", paste0(model, "_mixed.rds")))
# if (model == "base") {
#   ggsave(
#     file.path("plots", "Figure2.eps"), plot = fig_network,
#     width = 6, height = 7.5, dpi = 450
#   )
# }
