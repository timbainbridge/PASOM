################################# RQ2 Plot #####################################
#
# Some results from selected simulations. Produces the figure for RQ2 in the
# paper.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# WARNING: Slow to run

# Setup ------------------------------------------------------------------------

# Load packages
pkgs <- list(
  "igraph"       # For networks
  , "ggplot2"    # For all the figures
  , "paletteer"  # For colour palettes
  # , "ggpubr"     # For ggarrange()  # Does not work with ggMarginal()
  # , "ggExtra"    # For ggMarginal()  # Doesn't work with expand. F$%#$%k!
  # , "patchwork"  # Alternative for ggpubr  # Bug in guides = "collect"
  # , "gridExtra"  # Another alternative to ggarrange()
  , "ggnewscale"  # For different colours in density plots.
  , "cowplot"     # For plot_grid()  (4th time lucky?)
  # , "ggside"      # Same issue as ggExtra
  , "grid"        # For textGrob()
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Load figure function
source(file.path("scripts", "functions_fig.R"))

# Load data --------------------------------------------------------------------
# parset <- list(bt = c(1, .5, .1, .05, 0), gma = c(.2, .5, 1, 2))
bt <- c(1, .5, .1, .05, 0, -.05, -.5)
stc <- c(1, 5, 40)
# rq2_df <- lapply(
#   setNames(bt, nm = paste0("bt_", bt)),
#   function(m) {
#     lapply(
#       setNames(stc, nm = paste0("stc_", stc)),
#       function(n) {
#         model <- paste0("bt_", m, "_stc_", n)
#         tmp0 <- readRDS(file.path("output", model, "opinion1.rds"))
#         tmp1 <- readRDS(file.path("output", model, "nb_opinion.rds"))
#         tmp2 <- readRDS(file.path("output", model, "echo.rds"))
#         data.frame(
#           agent = lapply(tmp0, function(x) x[, ncol(x)]) |> unlist(),
#           neighbours = tmp1 |> unlist(),
#           echo = factor(
#             lapply(tmp2, function(x) x[, ncol(x)]) |> unlist(),
#             levels = -1:1,
#             labels = c("Anti-Science", "Neither", "Pro-Science")
#           )
#         )
#       }
#     )
#   }
# )
rq2_p <- lapply(
  setNames(bt, nm = paste0("bt_", bt)),
  function(m) {
    lapply(
      setNames(stc, nm = paste0("stc_", stc)),
      function(n) {
        model <- paste0("bt_", m, "_stc_", n)
        readRDS(file.path("plots", paste0(model, "_mixed.rds")))
      }
    )
  }
)
# leg <- rq2_df[[1]][[1]], leg = TRUE) |> get_legend()
# rq2_p <- lapply(rq2_df, function(m) lapply(m, dens.fun, alpha = .05))
p <- plot_grid(
  plotlist = c(
    list(
      plot_grid(
        textGrob(""),
        textGrob("Weak Priors", gp = gpar(fontface = "bold")),
        textGrob("Moderate Priors", gp = gpar(fontface = "bold")),
        textGrob("Strong Priors", gp = gpar(fontface = "bold")),
        nrow = 1,
        rel_widths = c(1, 4, 4, 4)
      )
    ),
    lapply(
      bt,
      function(x) {
        plot_grid(
          plotlist = c(
            list(
              textGrob(
                bquote(c["T"] == .(x * 1.25)), gp = gpar(fontface = "bold")
              )
            ),
            rq2_p[[paste0("bt_", x)]]
          ),
          ncol = 4,
          rel_widths = c(1, 4, 4, 4)
        )
      }
    )
  ),
  ncol = 1,
  rel_heights = c(1, rep(15, length(bt)))
) |>
  plot_grid(leg, rel_widths = c(15, 1))
ggdraw(p)
ggsave2(file.path("plots", "RQ2_plot.png"), p)
