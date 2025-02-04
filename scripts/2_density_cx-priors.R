################################# RQ2a Plot ####################################
#
# Some results from selected simulations. Produces the figure for RQ2a in the
# paper.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Setup ------------------------------------------------------------------------

# Load packages
pkgs <- list(
  "ggplot2"    # For all the figures
  , "cowplot"     # For plot_grid()  (4th time lucky?)
  , "grid"        # For textGrob()
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Load figure function
source(file.path("scripts", "functions_fig.R"))

# Load data --------------------------------------------------------------------
cx <- c(1, .5, .1, 0)
stc <- c(1, 5, 40)
rq2a_p <- lapply(
  setNames(cx, nm = paste0("cx_", cx)),
  function(m) {
    lapply(
      setNames(stc, nm = paste0("stc_", stc)),
      function(n) {
        model <- paste0("cx_", m, "_stc_", n)
        readRDS(file.path("plots", paste0(model, "_med.rds")))
      }
    )
  }
)

# Figure -----------------------------------------------------------------------
p0 <- plot_grid(
  plotlist = c(
    list(
      plot_grid(
        textGrob("Weak Priors", gp = gpar(fontface = "bold")),
        textGrob("Moderate Priors", gp = gpar(fontface = "bold")),
        textGrob("Strong Priors", gp = gpar(fontface = "bold")),
        nrow = 1
      )
    ),
    lapply(
      cx,
      function(x) plot_grid(plotlist = c(rq2a_p[[paste0("cx_", x)]]), ncol = 3)
    )
  ),
  ncol = 1,
  rel_heights = c(1, rep(10, length(cx)))
) |>
  # plot_grid(leg, rel_widths = c(15, 1))  # Here for when points were included.
  plot_grid(textGrob("Agents' opinions"), nrow = 2, rel_heights = c(50, 1))
py <- plot_grid(
  plotlist = c(
    list(textGrob("")),
    lapply(
      cx,
      function(x) textGrob(bquote(c["X"] == .(x)), gp = gpar(fontface = "bold"))
    )
  ),
  ncol = 1,
  rel_heights = c(1, rep(10, length(cx)))
)
p <- plot_grid(
  py, textGrob("Average of neighbours' opinions", rot = 90), p0,
  ncol = 3,
  rel_widths = c(3, 1, 50)
)
ggdraw(p)

# Save -------------------------------------------------------------------------
ggsave2(file.path("plots", "RQ2a_plot.png"), p, width = 13, height = 12)
ggsave2(file.path("plots", "Fig4.eps"), p, width = 6, height = 5.5)
