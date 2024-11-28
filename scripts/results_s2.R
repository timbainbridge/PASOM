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
  , "cowplot"     # For plot_grid()  (4th time lucky?)
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
    adjacent_vertices(i$g, V(i$g)$name),
    function(x) i$opinion1[V(i$g) %in% x] |> mean()
  )
  tmp <- data.frame(
    agent = i$opinion1,
    neighbours = tmp0,
    echo = factor(
      i$echo,
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
    xlab("Agents' opinions") +
    ylab("Average of neighbours' opinions") +
    scale_x_continuous(limits = 0:1, expand = rep(0, 4)) +
    scale_y_continuous(limits = 0:1, expand = rep(0, 4)) +
    theme(
      legend.position = ifelse(leg, "right", "none"),
      legend.key = element_rect(
        fill = paletteer_c("grDevices::Lajolla", 20, -1)[[2]]
      ),
      axis.title = element_text(size = 10)
    )
  p <- ggMarginal(p0, type = "densigram")
  return(p)
}

# Select simulations -----------------------------------------------------------
# mean_op0 <- readRDS(file.path("results", paste0(model, "_r.rds")))$opinion$mean
mean_op0 <- results$opinion$mean
mean_op <- mean_op0[nrow(mean_op0), ]
mixed_sel <- names(mean_op)[
  abs(mean_op - mean(mean_op)) == min(abs(mean_op - mean(mean_op)))
]
pros_sel <- names(mean_op)[mean_op == max(mean_op)]
anti_sel <- names(mean_op)[mean_op == min(mean_op)]

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
V(mixed$g)$color <- mixed$opinion1
V(pros$g)$color <- pros$opinion1
V(anti$g)$color <- anti$opinion1
# tmp0 <- list(
#   g = readRDS(file.path("output", model, "g.rds")),
#   opinion1 = readRDS(file.path("output", model, "opinion1.rds")),
#   echo = readRDS(file.path("output", model, "echo.rds"))
# )
# tmp0$opinion1 <- tmp0$opinion1[, ncol(tmp0$opinion1)]
# tmp0$echo <- tmp0$echo[, ncol(tmp0$echo)]
# tmp <- lapply(
#   setNames(nm = seq_along(tmp0$echo)),
#   function(x) {
#     list(g = tmp0$g[[x]], opinion1 = tmp0$opinion1[[x]], echo = tmp0$echo[[x]])
#   }
# )
# rm(tmp0)
# all <- lapply(
#   tmp,
#   function(x) {
#     tmp0 <- sapply(
#       V(x$g)$name,
#       function(y) {
#         tmp1 <- adjacent_vertices(x$g, y)[[1]]
#         x$opinion1[V(x$g) %in% tmp1] |> mean()
#       }
#     )
#     tmp1 <- data.frame(
#       agent = x$opinion1,
#       neighbours = tmp0,
#       echo = factor(
#         x$echo,
#         levels = -1:1,
#         labels = c("Anti-Science", "Neither", "Pro-Science")
#       )
#     ) |> do.call(rbind, args = _)
#   }
# )
# rm(tmp0, tmp)

# Create figures ---------------------------------------------------------------
leg1 <- get_legend(fig.fun(mixed$g, TRUE))
leg2 <- get_legend(dens.fun(mixed, TRUE))
fig_network <- plot_grid(
  leg1,
  plot_grid(
    fig.fun(g0), fig.fun(pros$g), fig.fun(anti$g), fig.fun(mixed$g),
    ncol = 1,
    labels = c("A", "C", "E", "G")
  ),
  plot_grid(
    dens.fun(begin), dens.fun(pros), dens.fun(anti), dens.fun(mixed),
    ncol = 1,
    labels = c("B", "D", "F", "H")
  ),
  leg2,
  ncol = 4,
  rel_widths = c(1, 6, 6, 2)
)
# } else {
#   fig_network <- grid.arrange(
#     fig.fun(pros$g), dens.fun(pros),
#     fig.fun(anti$g), dens.fun(anti),
#     fig.fun(mixed$g), dens.fun(mixed),
#     leg1, leg2,
#     widths = c(1, 5, 5, 1),
#     layout_matrix = rbind(c(NA, 1, 2, NA), c(7, 3, 4, 8), c(NA, 5, 6, NA))
#   )
# }
fig_mixed <- list(network = fig.fun(mixed$g), density = dens.fun(mixed))

# Remove unneeded large objects (slows things when in memory)
rm(anti, pros, mixed)
