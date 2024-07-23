############################ Single run focus ##################################
#
# Some results for selecting simulations. Produces Figures 2 and 5 from the
# paper.
#
################################################################################

# Setup ------------------------------------------------------------------------

# Load packages
pkgs <- list(
  "igraph"       # For networks
  , "ggplot2"    # For all the figures
  , "paletteer"  # For colour palettes
  , "ggpubr"     # For ggarrange()
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Create object directory (if it doesn't exist)
if (!dir.exists("plots")) dir.create("plots")

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
    ggtitle(paste0("(", i, ")"))
}
hist.fun <- function(p, i) {
  gg_hist <-
    ggplot(data.frame(ps = p), aes(x = ps)) +
    geom_histogram(
      breaks = 0:50*.02,
      fill = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 50, -1),
      colour = "black"
    ) +
    xlab("Opinion") +
    ylab("Count") +
    xlim(0:1) +
    theme_bw() +
    ggtitle(paste0("(", i, ")"))
}

# Load objects -----------------------------------------------------------------
# Opinion
source(file.path("scripts", "default_params.R"))
pers <- readRDS(file.path("input", "persi.rds"))
bs <- (stcon0 / (ths0)) * 1.5^-pers$i
as <- (stcon0 * (ths0)) * 1.5^pers$i
ps <- ((as - 1/3) / (as + bs - 2/3))

# Model outputs
models <- c(
  "base_np",
  "cx15_np"
  , "cx1_np"
  , "cx5_np"
  , "cx0_np"
)
rds <- rounds0
mean_op <- sapply(
  models,
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
pros_sel <- apply(
  mean_op,
  2,
  function(x) {
    names(x)[x == max(x)] |> sub(pattern = "iter", "", x = _) |> as.numeric()
  }
)
anti_sel <- apply(
  mean_op,
  2,
  function(x) {
    names(x)[x == min(x)] |> sub(pattern = "iter", "", x = _) |> as.numeric()
  }
)
obj_sel <- c("g", "opinion1")
g0 <- readRDS(file.path("output", "base_np.rds"))[[1]]$g0
V(g0)$color <- ps

names(ps) <- V(g0)$name
op <- ifelse(ps >= .6, 1, ifelse(ps <= .4, -1, 0))
echo00 <- mapply(
  function(x, y) {
    # Proportion of connections with pro-science view
    m <- (ps[y] >= .5) |> mean()
    # In pro (1) or anti (-1) science echo chamber or neither (0)?
    ifelse(x == 1 & m >= .9, 1, ifelse(x == -1 & m <= .1, -1, 0))
  },
  x = op, y = adjacent_vertices(g0, V(g0)$name)
)
mixed <- mapply(
  function(x, y) {
    tmp <- readRDS(file.path("output", paste0(x, ".rds")))[[y]][obj_sel]
    tmp1 <- tmp$opinion1[, rds]
    V(tmp$g)$color <- tmp1
    return(tmp$g)
  },
  x = models, y = mixed_sel, SIMPLIFY = FALSE
)
pros <- mapply(
  function(x, y) {
    tmp <- readRDS(file.path("output", paste0(x, ".rds")))[[y]][obj_sel]
    tmp1 <- tmp$opinion1[, rds]
    V(tmp$g)$color <- tmp1
    return(tmp$g)
  },
  x = models, y = pros_sel, SIMPLIFY = FALSE
)
anti <- mapply(
  function(x, y) {
    tmp <- readRDS(file.path("output", paste0(x, ".rds")))[[y]][obj_sel]
    tmp1 <- tmp$opinion1[, rds]
    V(tmp$g)$color <- tmp1
    return(tmp$g)
  },
  x = models, y = anti_sel, SIMPLIFY = FALSE
)

# Create figures ---------------------------------------------------------------
fig_base <- ggarrange(
  fig.fun(g0, "A"), hist.fun(V(g0)$color, "B"),
  fig.fun(pros$base_np, "C"), hist.fun(V(pros$base_np)$color, "D"),
  fig.fun(anti$base_np, "E"), hist.fun(V(anti$base_np)$color, "F"),
  fig.fun(mixed$base_np, "G"), hist.fun(V(mixed$base_np)$color, "H"),
  nrow = 4, ncol = 2,
  common.legend = TRUE, legend = "left"
)

rm(anti, pros)  # Remove unneeded large objects (these slow things down when in memory)

fig_compare0 <- mapply(
  function(x, y) {
    list(fig.fun(x, y[1]), hist.fun(V(x)$color, y[2]))
  },
  x = mixed,
  y = lapply(1:length(mixed), function(z) LETTERS[(z*2-1):(z*2)]),
  SIMPLIFY = FALSE
)

rm(mixed)  # Another large object to remove

fig_compare <-
  unlist(fig_compare0, FALSE) |>
  ggarrange(plotlist = _,
            ncol = 2, nrow = length(fig_compare0),
            common.legend = TRUE, legend = "left")
fig_compare_pptx <- sapply(
  fig_compare0,
  function(x) {
    x[[1]] + ggtitle("")
  },
  simplify = FALSE
) |>
  ggarrange(plotlist = _,
            nrow = 1, ncol = 5,
            common.legend = TRUE, legend = "left")

# Save figures -----------------------------------------------------------------
ggsave(file.path("plots", "fig_base.png"), plot = fig_base,
       width = 2000, height = 3500, units = "px")
ggsave(file.path("plots", "Figure2.eps"), plot = fig_base,
       width = 6, height = 7.5, dpi = 450)
ggsave(file.path("plots", "fig_compare.png"), plot = fig_compare,
       width = 2000, height = 4833, units = "px")
ggsave(file.path("plots", "Figure5.eps"), plot = fig_compare,
       width = 6, height = 7.5, dpi = 450)
ggsave(file.path("plots", "fig_compare_pptx.png"), plot = fig_compare_pptx,
       width = 4000, height = 1000, units = "px")
