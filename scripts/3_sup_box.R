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

# Save plots -------------------------------------------------------------------
ggsave2(
  file.path("plots", "sup_opm_box.pdf"), mboxplot,
  width = 17*1.75, height = 8.5*1.75, units = "cm"
)
ggsave2(
  file.path("plots", "sup_echo_box.pdf"), eboxplot,
  width = 17*1.75, height = 8.5*1.75, units = "cm"
)
