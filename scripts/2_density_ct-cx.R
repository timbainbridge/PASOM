################################# RQ2b Plot ####################################
#
# Some results from selected simulations. Produces the figure for RQ2b in the
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
bt <- c(1.25, .5, .1, 0, -.1, -.25, -.5)
rq2b_p <- lapply(
  setNames(bt, nm = paste0("bt_", bt)),
  function(m) {
    lapply(
      setNames(cx, nm = paste0("cx_", cx)),
      function(n) {
        if (m == 1.25) {
          model <- paste0("cx_", n, "_stc_40")
        } else {
          if (n == 1) {
            model <- paste0("bt_", m, "_stc_40")
          } else {
            model <- paste0("bt_", m, "_cx_", n)
          }
        }
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
        plotlist = c(
          lapply(
            cx,
            function(x) {
              textGrob(
                bquote(c["X"] == .(x)),
                gp = gpar(fontface = "bold", fontsize = 9)
              )
            }
          )
        ),
        nrow = 1
      )
    ),
    lapply(
      bt,
      function(x) plot_grid(plotlist = c(rq2b_p[[paste0("bt_", x)]]), ncol = 4)
    )
  ),
  ncol = 1,
  rel_heights = c(1.2, rep(9, length(bt)))
) |>
  # plot_grid(leg, rel_widths = c(15, 1))  # Here for when points were included.
  plot_grid(
    textGrob("Agents' opinions", gp = gpar(fontsize = 9)),
    nrow = 2,
    rel_heights = c(50, 1.4)
  )
py <- plot_grid(
  plotlist = c(
    list(textGrob("")),
    lapply(
      bt,
      function(x) {
        textGrob(
          bquote(c["T"] == .(x)), gp = gpar(fontface = "bold", fontsize = 9)
        )
      }
    )
  ),
  ncol = 1,
  rel_heights = c(1.4, rep(9, length(bt)))
)
p <- plot_grid(
  py,
  textGrob(
    "Average of neighbours' opinions",
    rot = 90,
    gp = gpar(fontsize = 9)
  ),
  p0,
  ncol = 3,
  rel_widths = c(4.5, 1, 50)
)
ggdraw(p)

# Save -------------------------------------------------------------------------
ggsave2(file.path("plots", "RQ2b_plot.png"), p, width = 13, height = 12)
ggsave2(
  file.path("plots", "Fig7.eps"), p,
  width = 2250, height = 2075, units = "px", dpi = 300
)
