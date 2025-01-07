############################# Figure Functions #################################
#
# Code to produce the density plots.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Libraries required for functions ---------------------------------------------
pkgs <- c(
  "ggplot2"
  , "grid"  # For is.grob()
  , "ggnewscale"  # For different colours in density plots.
  , "cowplot"     # For axis histograms
  , "paletteer"  # For colour palettes
  , "igraph"       # For networks
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Functions to create figures ##################################################

# For density plot -------------------------------------------------------------
dens.fun <- function(i, leg = FALSE, point = TRUE, alpha = 1, labs0 = TRUE) {
  p0 <- ggplot(i, aes(x = agent, y = neighbours)) +
    geom_density2d_filled(adjust = .5, bins = 20) +
    scale_fill_manual(
      values = paletteer_c("grDevices::Lajolla", 20, -1),
      guide = "none"
    )
  if (point) {
    p0 <- p0 +
      new_scale_colour() +
      geom_point(
        aes(x = agent, y = neighbours, colour = echo),
        i,
        alpha = alpha,
        size = .2
      ) +
      scale_colour_manual(
        values = setNames(
          # paletteer_c("ggthemes::Green-Blue Diverging", 5)[2:4],
          c("#80C673", "#F4F8FB", "#8DBBDC"),
          c("Anti-Science", "Neither", "Pro-Science")
        ),
        guide = guide_legend(
          override.aes = list(size=2),
          title = "Echo Chamber\nMembership"
        )
      )
  }
  p1 <- p0 +
    theme_bw() +
    scale_x_continuous(limits = 0:1, expand = rep(0, 4)) +
    scale_y_continuous(limits = 0:1, expand = rep(0, 4)) +
    theme(
      legend.position = if (leg) "right" else "none",
      legend.key = element_rect(
        fill = paletteer_c("grDevices::Lajolla", 20, -1)[[2]]
      ),
      axis.title = element_text(size = 10)
    )
  if (labs0) {
    p <- p1 +
      labs(x = "Agents' opinions", y = "Average of neighbours' opinions")
  } else {
    p <- p1 + labs(x = NULL, y = NULL)
  }
  p_xm <- axis_canvas(p, axis = "x") +
    geom_histogram(
      data = i,
      mapping = aes(x = agent),
      colour = "black",
      fill = "gray",
      breaks = 0:40*.025
    )
  p_ym <- axis_canvas(p, axis = "y") +
    geom_histogram(
      data = i,
      mapping = aes(y = neighbours),
      colour = "black",
      fill = "gray",
      breaks = 0:40*.025
    )
  insert_xaxis_grob(p, p_xm, position = "top") |>
    insert_yaxis_grob(p_ym, position = "right")
}
dens.fun2 <- function(i, leg = FALSE, point = TRUE, alpha = 1, labs0 = TRUE) {
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
  dens.fun(tmp, leg = leg, point = point, alpha = alpha, labs0 = labs0)
}

# For network graph ------------------------------------------------------------
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
    geom_segment(
      aes(x = from.x1, xend = to.x1, y = from.x2, yend = to.x2),
      g_edges,
      colour="darkgray",
      linewidth = .2
    ) +
    geom_point(aes(x = X1, y = X2), g_coord, size = 1.5, colour = "black") +
    geom_point(aes(x = X1, y = X2, colour = Opinion), g_coord) +
    scale_colour_gradientn(
      colours = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 101, -1),
      limits = 0:1,
      breaks = 0:2*.5,
      labels = 0:2*.5
    ) +
    theme_void() +
    theme(legend.position = ifelse(leg, "right", "none"))
}

# Temporary fix to cowplot's get_legend ----------------------------------------
# Issue reported at and (modified) solution from:
# https://github.com/wilkelab/cowplot/issues/202
get_legend2 <- function(plot, legend = NULL) {
  if (is.ggplot(plot)) {
    gt <- ggplotGrob(plot)
  } else {
    if (is.grob(plot)) {
      gt <- plot
    } else {
      stop("Plot object is neither a ggplot nor a grob.")
    }
  }
  pattern <- "guide-box"
  if (!is.null(legend)) {
    pattern <- paste0(pattern, "-", legend)
  }
  indices <- grep(pattern, gt$layout$name)
  not_empty <- !vapply(
    gt$grobs[indices],
    inherits, what = "zeroGrob",
    FUN.VALUE = logical(1)
  )
  indices <- indices[not_empty]
  if (length(indices) > 0) {
    return(gt$grobs[[indices[1]]])
  }
  return(NULL)
}
