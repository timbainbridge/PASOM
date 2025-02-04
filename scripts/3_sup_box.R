############################ Supplement boxplots ###############################
#
# Produces the boxplots found in the supplement.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Setup ------------------------------------------------------------------------
# Load packages
pkgs <- list(
  "ggplot2"      # For all the figures
  , "cowplot"     # For ggarrange()
  , "reshape"    # For melt()
  , "paletteer"  # For palettes
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Create object directory (if it doesn't exist)
if (!dir.exists("plots")) dir.create("plots")

# Custom functions
source(file.path("scripts", "functions_fig.R"))

# Data -------------------------------------------------------------------------

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
results_names <- c(
  "lower", "upper", "pseudomedian", "lowert", "uppert", "mean", "median",
  "round"
)
results_sup <- sapply(
  models_sup,
  function(y) {
    sapply(
      y,
      function(x) {
        tmp <- readRDS(file.path("results", paste0(x, "_r.rds")))
        echo20 <- (tmp$echo$echo1[200, ] + tmp$echo$echo0[200, ]) * 100
        echo2w <- wilcox.test(echo20, conf.int = TRUE, mu = -.001)
        echo21 <- echo2w$conf.int
        echo2pm <- echo2w$estimate
        echo2t <- t.test(echo20)$conf.int
        echo2m <- echo20 |> mean()
        echo2md <- echo20 |> median()
        echo2 <- c(echo21, echo2pm, echo2t, echo2m, echo2md, 200)
        names(echo2) <- results_names
        echo10 <- tmp$echo$echo1[200, ] * 100
        echo1w <- wilcox.test(echo10, conf.int = TRUE, mu = -.001)
        echo11 <- echo1w$conf.int
        echo1pm <- echo1w$estimate
        echo1t <- t.test(echo10)$conf.int
        echo1m <- echo10 |> mean()
        echo1md <- echo10 |> median()
        echo1 <- c(echo11, echo1pm, echo1t, echo1m, echo1md, 200)
        names(echo1) <- results_names
        echo00 <- tmp$echo$echo0[200, ] * 100
        echo0w <- wilcox.test(echo00, conf.int = TRUE, mu = -.001)
        echo01 <- echo0w$conf.int
        echo0pm <- echo0w$estimate
        echo0t <- t.test(echo00)$conf.int
        echo0m <- echo00 |> mean()
        echo0md <- echo00 |> median()
        echo0 <- c(echo01, echo0pm, echo0t, echo0m, echo0md, 200)
        names(echo0) <- results_names
        opm0 <- tmp$opinion$mean[200, ]
        opmw <- wilcox.test(opm0, conf.int = TRUE)
        opm1 <- opmw$conf.int
        opmpm <- opmw$estimate
        opmt <- t.test(opm0)$conf.int
        opmm <- opm0 |> mean()
        opmmd <- opm0 |> median()
        opm <- c(opm1, opmpm, opmt, opmm, opmmd, 200)
        names(opm) <- results_names
        ppro <- (tmp$opinion$final > .5) |> colMeans()
        panti <- (tmp$opinion$final < .5) |> colMeans()
        homophily <- tmp$homophily
        sdop <- tmp$opinion$sd[200, ]
        list(
          echo1 = echo1, echo10 = echo10, echo0 = echo0, echo00 = echo00,
          echo2 = echo2, echo20 = echo20, opm = opm, opm0 = opm0,
          homophily = homophily, sd = sdop, ppro = ppro, panti = panti
        )
      },
      simplify = FALSE
    )
  },
  simplify = FALSE
)

# Opinion ----------------------------------------------------------------------
mdata_sup <- do.call(
  rbind,
  sapply(
    unlist(results_sup, recursive = FALSE),
    function(x) x$opm, simplify = FALSE
  )
) |>
  data.frame()
mdata_sup0 <- mapply(
  function(y, z, zn) {
    cbind(
      melt(sapply(y, function(x) x$opm0) |> data.frame()),
      iter = rep(1:200, length(z)),
      simulation = factor(rep(z, each = 200), levels = z),
      group = zn
    ) |>
      data.frame()
  },
  y = results_sup, z = model_names, zn = group_names, SIMPLIFY = FALSE
) |>
  do.call(rbind, args = _)
mdata_sup0$group <-
  factor(mdata_sup0$group, levels = group_names, ordered = TRUE)

# # Anti-science echo chambers ---------------------------------------------------
# e0data_sup <- do.call(
#   rbind,
#   sapply(results_sup, function(x) x$echo0, simplify = FALSE)
# ) |> data.frame()
# e0data_sup0 <- cbind(
#   melt(sapply(results_sup, function(x) x$echo00) |> data.frame()),
#   iter = rep(1:200, length(model_names)),
#   simulation = factor(rep(model_names, each = 200), levels = model_names)
# ) |> data.frame()
# names(e0data_sup0)[names(e0data_sup0) == "value"] <- "mean"
# 
# # Pro-science echo chambers ---------------------------------------------------
# e1data_sup <- do.call(
#   rbind,
#   sapply(results_sup, function(x) x$echo1, simplify = FALSE)
# ) |> data.frame()
# e1data_sup0 <- cbind(
#   melt(sapply(results_sup, function(x) x$echo10) |> data.frame()),
#   iter = rep(1:200, length(model_names)),
#   simulation = factor(rep(model_names, each = 200), levels = model_names)
# ) |> data.frame()
# names(e1data_sup0)[names(e1data_sup0) == "value"] <- "mean"

# Any echo chamber -------------------------------------------------------------
edata_sup <- do.call(
  rbind,
  sapply(
    unlist(results_sup, recursive = FALSE),
    function(x) x$echo2, simplify = FALSE
  )
) |>
  data.frame()
edata_sup0 <- mapply(
  function(y, z, zn) {
    cbind(
      melt(sapply(y, function(x) x$echo20) |> data.frame()),
      iter = rep(1:200, length(z)),
      simulation = factor(rep(z, each = 200), levels = z),
      group = zn
    ) |>
      data.frame()
  },
  y = results_sup, z = model_names, zn = group_names, SIMPLIFY = FALSE
) |>
  do.call(rbind, args = _)
edata_sup0$group <-
  factor(edata_sup0$group, levels = group_names, ordered = TRUE)

# Box plots --------------------------------------------------------------------
# Opinion
mboxplot <- box_custom2(
  mdata_sup0, x = simulation, y = value, group = group,
  "Type of Change", "Simulation", "Mean opinion",
  limits = c(0, 1),
  pal = c(
    "Black",
    paletteer_c("ggthemes::Blue", 5)[2:5],
    paletteer_c("ggthemes::Green", 5)[2:5],
    paletteer_c("ggthemes::Classic Area-Brown", 4)[2:3],
    paletteer_c("ggthemes::Red", 3)[2:3],
    paletteer_c("ggthemes::Purple", 3)[2:3],
    paletteer_c("ggthemes::Gray", 4)[2:4]
  )
)
mboxplot

# # Anti-science echo chambers ---------------------------------------------------
# e0boxplot <-
#   ggplot(e0data_sup0, aes(x = simulation, y = mean)) +
#   geom_boxplot(alpha = 0) +
#   geom_point(aes(x = simulation, y = mean, colour = simulation)
#              , alpha = .5
#              , position = position_jitterdodge(jitter.width = .1)) +
#   labs(x = "Simulation", colour = "Simulation") +
#   scale_y_continuous("% agents in an anti-science\nEcho Chamber",
#                      0:5*20,
#                      0:4*20+10) +
#   coord_cartesian(ylim = c(0, 100), expand = FALSE) +
#   theme_bw() +
#   theme(text = element_text(size = 10),
#         plot.margin = unit(c(.5, .5, .5, .5), "cm"),
#         legend.position = "none") +
#   ggtitle("(A)")
# e0boxplot
# 
# # Pro-science echo chambers ---------------------------------------------------
# e1boxplot <-
#   ggplot(e1data_sup0, aes(x = simulation, y = mean)) +
#   geom_boxplot(alpha = 0) +
#   geom_point(aes(x = simulation, y = mean, colour = simulation)
#              , alpha = .5
#              , position = position_jitterdodge(jitter.width = .1)) +
#   labs(x = "Simulation", colour = "Simulation") +
#   scale_y_continuous("% agents in a pro-science\nEcho Chamber",
#                      0:5*20,
#                      0:4*20+10) +
#   coord_cartesian(ylim = c(0, 100), expand = FALSE) +
#   theme_bw() +
#   theme(text = element_text(size = 10),
#         plot.margin = unit(c(.5, .5, .5, .5), "cm"),
#         legend.position = "none") +
#   ggtitle("(B)")
# e1boxplot

# Any echo chamber -------------------------------------------------------------
eboxplot <- box_custom2(
  edata_sup0, x = simulation, y = value, group = group,
  "Type of Change", "Simulation", "% agents in an echo chamber",
  limits = c(0, 100),
  pal = c(
    "Black",
    paletteer_c("ggthemes::Blue", 5)[2:5],
    paletteer_c("ggthemes::Green", 5)[2:5],
    paletteer_c("ggthemes::Classic Area-Brown", 4)[2:3],
    paletteer_c("ggthemes::Red", 3)[2:3],
    paletteer_c("ggthemes::Purple", 3)[2:3],
    paletteer_c("ggthemes::Gray", 4)[2:4]
  )
)
eboxplot
#   ggplot(edata_sup0, aes(x = simulation, y = mean)) +
#   geom_boxplot(outlier.alpha = 0) +
#   geom_point(aes(x = simulation, y = mean, colour = simulation)
#              , alpha = .5
#              , position = position_jitterdodge(jitter.width = .1)) +
#   labs(x = "Simulation", colour = "Simulation") +
#   scale_y_continuous("% agents in any\nEcho Chamber",
#                      0:5*20,
#                      0:4*20+10) +
#   coord_cartesian(ylim = c(0, 100), expand = FALSE) +
#   theme_bw() +
#   theme(text = element_text(size = 10),
#         plot.margin = unit(c(.5, .5, .5, .5), "cm"),
#         legend.position = "none") +
#   ggtitle("(C)")
# eboxplot
#  
# eboxall <- ggarrange(plotlist = list(e0boxplot, e1boxplot, eboxplot),
#                      nrow = 3,
#                      legend = "none")
# eboxall

# # Number of toxic shares -------------------------------------------------------
# results_sh_sup <- sapply(
#   models_sup,
#   function(x) {
#     sapply(readRDS(file.path("results", paste0(x, "_r.rds")))$shares, colSums)
#   },
#   simplify = FALSE
# )
# shdata_sup <- sapply(
#   results_sh_sup,
#   function(x) {
#     tmp <- data.frame(x)
#     data.frame(tx = tmp$tx11 + tmp$tx10, a = tmp$a11 + tmp$a10)
#   },
#   simplify = FALSE
# )

# # Tx ---------------------------------------------------------------------------
# txdata_sup <- cbind(
#   melt(sapply(shdata_sup, function(x) x$tx) |> data.frame()),
#   iter = rep(1:200, length(model_names)),
#   simulation = factor(rep(model_names, each = 200), levels = model_names)
# ) |> data.frame()
# names(txdata_sup)[names(txdata_sup) == "value"] <- "shares"
# txplot <-
#   ggplot(txdata_sup, aes(x = simulation, y = shares)) +
#   geom_boxplot(notch = TRUE, alpha = 0) +
#   geom_point(aes(x = simulation, y = shares, colour = simulation)
#              , alpha = .5
#              , position = position_jitterdodge(jitter.width = .1)) +
#   labs(x = "Simulation", y = "Toxic Shares", colour = "Simulation") +
#   theme_bw() +
#   theme(text = element_text(size = 10),
#         plot.margin = unit(c(.5, .5, .5, .5), "cm"),
#         legend.position = "none") +
#   ggtitle("(A)")
# txplot
# 
# # A ---------------------------------------------------------------------------
# adata_sup <- cbind(
#   melt(sapply(shdata_sup, function(x) x$a) |> data.frame()),
#   iter = rep(1:200, length(model_names)),
#   simulation = factor(rep(model_names, each = 200), levels = model_names)
# ) |> data.frame()
# names(adata_sup)[names(adata_sup) == "value"] <- "shares"
# aplot <-
#   ggplot(adata_sup, aes(x = simulation, y = shares)) +
#   geom_boxplot(notch = TRUE, alpha = 0) +
#   geom_point(aes(x = simulation, y = shares, colour = simulation)
#              , alpha = .5
#              , position = position_jitterdodge(jitter.width = .1)) +
#     labs(x = "Simulation", y = "Constructive Shares", colour = "Simulation") +
#   theme_bw() +
#   theme(text = element_text(size = 10),
#         plot.margin = unit(c(.5, .5, .5, .5), "cm"),
#         legend.position = "none") +
#   ggtitle("(B)")
# aplot

# # Combined Tx and A plots ------------------------------------------------------
# txaplot <- ggarrange(plotlist = list(txplot, aplot),
#                      nrow = 2,
#                      legend = "none")
# txaplot
# 
# # SD plot ----------------------------------------------------------------------
# sddata0 <- do.call(
#   rbind,
#   mapply(
#     function(x, y) {
#       data.frame(simulation = factor(rep(y, length(x$sd)), levels = y),
#                  sd = x$sd)
#     },
#     x = results_sup, y = model_names, SIMPLIFY = FALSE
#   )
# )
# sdboxplot <-
#   ggplot(sddata0, aes(x = simulation, y = sd)) +
#   geom_boxplot(
#     # notch = TRUE,
#     alpha = 0
#   ) +
#   geom_point(aes(x = simulation, y = sd, colour = simulation)
#              , alpha = .5
#              , position = position_jitterdodge(jitter.width = .1)) +
#   labs(x = "Simulation", colour = "Simulation") +
#   scale_y_continuous("SD of opinions") +
#   theme_bw() +
#   theme(text = element_text(size = 10),
#         plot.margin = unit(c(.5, .5, .5, .5), "cm"),
#         legend.position = "none") +
#   ggtitle("(B)")
# sdboxplot
# 
# msdarrange <- ggarrange(plotlist = list(mboxplot, sdboxplot),
#                         nrow = 2,
#                         legend = "none")
# msdarrange

# Save plots -------------------------------------------------------------------
# saveRDS(eboxall, file.path("plots", "sup_echo_plot.rds"))
# saveRDS(txaplot, file.path("plots", "sup_txa_plot.rds"))
# saveRDS(msdarrange, file.path("plots", "sup_msd_plot.rds"))

ggsave2(
  file.path("plots", "sup_opm_box.pdf"), mboxplot,
  width = 17*1.75, height = 8.5*1.75, units = "cm"
)
ggsave2(
  file.path("plots", "sup_echo_box.pdf"), eboxplot,
  width = 17*1.75, height = 8.5*1.75, units = "cm"
)
