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
  , "cowplot"     #
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Create object directory (if it doesn't exist
if (!dir.exists("plots")) dir.create("plots")

# Custom functions
source(file.path("scripts", "functions_fig.R"))

# Load objects -----------------------------------------------------------------

# Model outputs
models_sup <- list(
  base = "base",
  network = c("g0_1", "g0_2", "lattice", "g1000"),
  sd = c("sd0.5", "sd2", "sdi", "sdj"),
  ck = c("ck0.8", "ck1.25"),
  cb = c("cb0.5", "cb2"),
  d = c("d0.5", "d0"),
  u = c("u0", "u0.4", "u0.8")
)
model_names <- list(
  Base = "Base",
  Network = c("Network 1", "Network 2", "Lattice Network", "1000 Agents"),
  SD = c("Halved SD", "Doubled SD", "Individual SD only", "Grouped SD only"),
  cK = c("Friend cost reduced", "Friend cost increased"),
  cB = c("Friend benefit reduced", "Friend benefit increased"),
  d = c("Discount halved", "Myopic"),
  u = c(
    "Payoff difference elimiated",
    "Payoff difference doubled",
    "Payoff difference quadrupled"
  )
)
group_names <- c(
  "Base", "Network", "SD", "Friend\nCost", "Friend\nBenefit", "Discount",
  "Payoff\nDifference"
)

# Default parameters (for rounds0)
source(file.path("scripts", "default_params.R"))

rds <- rounds0
mean_op <- sapply(
  models_sup,
  function(y) {
    sapply(
      y,
      function(x) {
        tmp <- readRDS(file.path("results", paste0(x, "_r.rds")))
        tmp[["opinion"]][["mean"]][rds, ]
      }
    )
  }
)
mixed_sel <- lapply(
  mean_op,
  function(y) {
    apply(
      y,
      2,
      function(x) {
        names(x)[abs(x - .5) == min(abs(x - .5))] |>
          sub(pattern = "iter", "", x = _) |>
          as.numeric()
      }
    )
  }
)
obj_sel <- c("g", "opinion1", "echo")
mixed <- mapply(
  function(i, j) {
    mapply(
      function(x, y) {
        tmp <- lapply(
          setNames(nm = obj_sel),
          function(z) readRDS(file.path("output", x, paste0(z, ".rds")))[[y]]
        )
        tmp1 <- tmp
        tmp1$opinion1 <- tmp$opinion1[, rds]
        V(tmp1$g)$color <- tmp1$opinion1
        tmp1$echo <- tmp$echo[, rds]
        dens <- dens.fun2(tmp1, labs0 = FALSE)
        # dens <- readRDS(file.path("plots", paste0(x, "_med.rds")))
        return(list(g = tmp$g, dens = dens))
      },
      x = i, y = j, SIMPLIFY = FALSE
    )
  },
  i = models_sup, j = mixed_sel, SIMPLIFY = FALSE
)

# Function to create figures ---------------------------------------------------
# fig.fun <- function(g, i) {
#   set.seed(0)  # For layout_nicely()
#   g_coord <- data.frame(agent = V(g)$name,
#                         layout_nicely(g),
#                         Opinion = V(g)$color)
#   g_edges <- as_data_frame(g)
#   g_edges$from.x1 <- g_coord$X1[match(g_edges$from, g_coord$agent)]
#   g_edges$from.x2 <- g_coord$X2[match(g_edges$from, g_coord$agent)]
#   g_edges$to.x1 <- g_coord$X1[match(g_edges$to, g_coord$agent)]
#   g_edges$to.x2 <- g_coord$X2[match(g_edges$to, g_coord$agent)]
#   gg_g <-
#     ggplot() +
#     geom_segment(aes(x = from.x1, xend = to.x1, y = from.x2, yend = to.x2),
#                  g_edges,
#                  colour="black",
#                  alpha = .2,
#                  linewidth = .4) +
#     geom_point(aes(x = X1, y = X2), g_coord, size = 1.5, colour = "black") +
#     geom_point(aes(x = X1, y = X2, colour = Opinion), g_coord) +
#     scale_colour_gradientn(
#       colours = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 101, -1),
#       limits = 0:1,
#       breaks = 0:2*.5,
#       labels = 0:2*.5
#     ) +
#     theme_void() +
#     ggtitle(paste0(i))
# }
# hist.fun <- function(p, i) {
#   gg_hist <-
#     ggplot(data.frame(ps = p), aes(x = ps)) +
#     geom_histogram(
#       breaks = 0:50*.02, alpha = .8,
#       fill = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 50, -1),
#       colour = "black"
#     ) +
#     xlab("Opinion") +
#     ylab("Count") +
#     xlim(0:1) +
#     theme_bw() +
#     ggtitle(paste0(i))
# }

# Create figures ---------------------------------------------------------------
# leg1 <- fig.fun(mixed$base$g, TRUE) |> get_legend2()
# fig_compare_n0 <- lapply(mixed, function(x) fig.fun(x$g))
# fig_compare_n <- plot_grid(
#   leg1,
#   plot_grid(
#     plotlist = fig_compare_n0,
#     ncol = 3,
#     labels = model_names,
#     scale = 0.9
#     # , label_x = 1
#   ),
#   ncol = 2,
#   rel_widths = c(1, 33)
# )
# fig_compare_n
leg2 <- readRDS(file.path("plots", "legend.rds"))
p0 <- plot_grid(
  plotlist = mapply(
    function(y, z, x) {
      plot_grid(
        plotlist = lapply(y, function(i) i$dens),
        ncol = 4,
        labels = z,
        label_size = 11,
        label_x = .1,
        hjust = -.08,
        scale = .95
      )
    },
    y = mixed, z = model_names, x = group_names, SIMPLIFY = FALSE
  ),
  ncol = 1
)
# p0 <- plot_grid(
#   plotlist = lapply(
#     mixed, function(x) x$dens
#   ),
#   ncol = 3,
#   labels = model_names,
#   label_size = 11,
#   label_x = .1,
#   hjust = -.08,
#   scale = .95
# )

p1 <- plot_grid(
  plot_grid(
    plotlist = c(
      list(textGrob("Base", gp = gpar(fontface = "bold"))),
      lapply(
        group_names[-1],
        function(x) {
          textGrob(paste0(x, "\nChanges"), gp = gpar(fontface = "bold"))
        }
      )
    ),
    # textGrob("Base", gp = gpar(fontface = "bold")),
    # textGrob("Network\nChanges", gp = gpar(fontface = "bold")),
    # textGrob("SD\nChanges", gp = gpar(fontface = "bold")),
    # textGrob("Friend\nCost\nChanges", gp = gpar(fontface = "bold")),
    # textGrob("Friend\nBenefit\nChanges", gp = gpar(fontface = "bold")),
    # textGrob("Discount\nChanges", gp = gpar(fontface = "bold")),
    # textGrob("Payoff\ndifference\nChanges", gp = gpar(fontface = "bold")),
    ncol = 1
  ),
  textGrob("Average of neighbours' opinions", rot = 90), p0,
  ncol = 3, rel_widths = c(5, 1, 50), scale = 1.01
)
p2 <- plot_grid(
  NULL, p1, textGrob("Agents' opinions"),
  nrow = 3, rel_heights = c(.4, 40, 1.2), scale = 1.01
)
p <- ggdraw() + draw_plot(p2) + draw_grob(leg2, .35, -.1)
p

# Save figures -----------------------------------------------------------------
# saveRDS(fig_compare_n, file.path("plots", "sup_compare_n.rds"))
ggsave2(
  file.path("plots", "sup_compare_d.pdf"),
  p,
  width = 17*1.75, height = 23*1.75, units = "cm"
)
