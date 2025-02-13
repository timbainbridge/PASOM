################ A file to produce boxplots for the paper ######################
#
# This file produces Figures 5 and 6 from the paper + an ad hoc comparison of
# the number of toxic posts for 2 selected parameter sets in the Discussion. 
# 
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Set up -----------------------------------------------------------------------
# Packages
pkgs <- list(
  "ggplot2"       # For all the figures
  , "reshape"     # For melt()
  , "paletteer"   # For palettes
  , "psych"       # For describe()
  , "cowplot"     # For plot_grid()
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Custom functions
source(file.path("scripts", "functions_fig.R"))

# Data -------------------------------------------------------------------------
bt <- c(1.25, .5, .1, 0, -.1, -.25, -.5)
cx <- c(1, .5, .1, 0)
stcn <- c("Weak", "Moderate", "Strong")
stc <- c(1, 5, 40)
models2 <- c(
  "base",
  sapply(bt, function(x) sapply(stc, function(y) paste0("bt_", x, "_stc_", y))),
  sapply(cx, function(x) sapply(stc, function(y) paste0("cx_", x, "_stc_", y))),
  sapply(
    bt[-1], function(x) sapply(cx[-1], function(y) paste0("bt_", x, "_cx_", y))
  )
)
model_params <- data.frame(
  bt = c(
    1.25,
    sapply(bt, function(x) sapply(stcn, function(y) x)),
    sapply(cx, function(x) sapply(stcn, function(y) 1.25)),
    sapply(bt[-1], function(x) sapply(cx[-1], function(y) x))
  ),
  cx = c(
    1,
    sapply(bt, function(x) sapply(stcn, function(y) 1)),
    sapply(cx, function(x) sapply(stcn, function(y) x)),
    sapply(bt[-1], function(x) sapply(cx[-1], function(y) y))
  ),
  stc = c(
    "Weak",
    sapply(bt, function(x) sapply(stcn, function(y) y)),
    sapply(cx, function(x) sapply(stcn, function(y) y)),
    sapply(bt[-1], function(x) sapply(cx[-1], function(y) "Strong"))
  )
)
results2 <- sapply(
  models2,
  function(x) {
    tmp <- readRDS(file.path("results", paste0(x, "_r.rds")))
    rnd <- nrow(tmp$echo$echo1)
    echo20 <- (tmp$echo$echo1[rnd, ] + tmp$echo$echo0[rnd, ]) * 100
    echo2w <- wilcox.test(echo20, conf.int = TRUE, mu = -.001)
    echo21 <- echo2w$conf.int
    echo2pm <- echo2w$estimate
    echo2t <- t.test(echo20)$conf.int
    echo2m <- echo20 |> mean()
    echo2md <- echo20 |> median()
    echo2 <- c(echo21, echo2pm, echo2t, echo2m, echo2md, rnd)
    nms <- c(
      "lower", "upper", "pseudomedian", "lowert", "uppert", "mean", "median",
      "round"
    )
    names(echo2) <- nms
    echo10 <- tmp$echo$echo1[rnd, ] * 100
    echo1w <- wilcox.test(echo10, conf.int = TRUE, mu = -.001)
    echo11 <- echo1w$conf.int
    echo1pm <- echo1w$estimate
    echo1t <- t.test(echo10)$conf.int
    echo1m <- echo10 |> mean()
    echo1md <- echo10 |> median()
    echo1 <- c(echo11, echo1pm, echo1t, echo1m, echo1md, rnd)
    names(echo1) <- nms
    echo00 <- tmp$echo$echo0[rnd, ] * 100
    echo0w <- wilcox.test(echo00, conf.int = TRUE, mu = -.001)
    echo01 <- echo0w$conf.int
    echo0pm <- echo0w$estimate
    echo0t <- t.test(echo00)$conf.int
    echo0m <- echo00 |> mean()
    echo0md <- echo00 |> median()
    echo0 <- c(echo01, echo0pm, echo0t, echo0m, echo0md, rnd)
    names(echo0) <- nms
    opm0 <- tmp$opinion$mean[rnd, ]
    opmw <- wilcox.test(opm0, conf.int = TRUE)
    opm1 <- opmw$conf.int
    opmpm <- opmw$estimate
    opmt <- t.test(opm0)$conf.int
    opmm <- opm0 |> mean()
    opmmd <- opm0 |> median()
    opm <- c(opm1, opmpm, opmt, opmm, opmmd, rnd)
    names(opm) <- nms
    ppro <- (tmp$opinion$final > .5) |> colMeans()
    panti <- (tmp$opinion$final < .5) |> colMeans()
    homophily <- tmp$homophily
    sdop <- tmp$opinion$sd[rnd, ]
    list(
      echo1 = echo1, echo10 = echo10, echo0 = echo0, echo00 = echo00,
      echo2 = echo2, echo20 = echo20, opm = opm, opm0 = opm0,
      homophily = homophily, sd = sdop, ppro = ppro, panti = panti
    )
  },
  simplify = FALSE
)

# Data for plots ---------------------------------------------------------------

# Echo chambers
ebox_agg <- cbind(
  sapply(results2, function(x) x$echo2[c("mean", "lowert", "uppert")]) |>
    t() |>
    data.frame(),
  lapply(model_params, as.factor) |> data.frame()
)
ebox_agg$stc <- factor(ebox_agg$stc, levels = stcn)  # Reorder
ebox_d0 <- cbind.data.frame(
  melt(sapply(results2, function(x) x$echo20) |> data.frame()),
  iter = rep(1:200, length(results2)),
  bt = rep(ebox_agg$bt[match(names(results2), rownames(ebox_agg))], each = 200),
  cx = rep(ebox_agg$cx[match(names(results2), rownames(ebox_agg))], each = 200),
  stc =
    rep(ebox_agg$stc[match(names(results2), rownames(ebox_agg))], each = 200)
)

# Select simulations
ebox_d <-
  ebox_d0[ebox_d0$stc == "Strong" & !ebox_d0$variable == "cx_1_stc_40", ]
ebox_d_cx <- ebox_d0[grep("cx_.+_stc", ebox_d0$variable), ]
ebox_d_bt <- ebox_d0[grep("bt_.+_stc", ebox_d0$variable), ]

# Opinion
opbox_agg <- cbind(
  sapply(results2, function(x) x$opm[c("mean", "lowert", "uppert")]) |>
    t() |>
    data.frame(),
  lapply(model_params, as.factor) |> data.frame()
)
opbox_agg$stc <- factor(opbox_agg$stc, levels = stcn)  # Reorder
opbox_d0 <- cbind.data.frame(
  melt(sapply(results2, function(x) x$opm0) |> data.frame()),
  iter = rep(1:200, length(results2)),
  bt =
    rep(opbox_agg$bt[match(names(results2), rownames(opbox_agg))], each = 200),
  cx =
    rep(opbox_agg$cx[match(names(results2), rownames(opbox_agg))], each = 200),
  stc =
    rep(opbox_agg$stc[match(names(results2), rownames(opbox_agg))], each = 200)
)

# Select simulations
opbox_d <-
  opbox_d0[opbox_d0$stc == "Strong" & !opbox_d0$variable == "cx_1_stc_40", ]
opbox_d_cx <- opbox_d0[grep("cx_.+_stc", opbox_d0$variable), ]
opbox_d_bt <- opbox_d0[grep("bt_.+_stc", opbox_d0$variable), ]

# Figures ----------------------------------------------------------------------

# Echo chambers
ebox_p <- box_custom(
  ebox_d, x = cx, y = value, colour = bt,
  bquote(c[X]), bquote(c[T]), "% agents in an echo chamber",
  limits = c(0, 101)
)
# ebox_p
ebox_p_cx <- box_custom(
  ebox_d_cx, colour = cx, y = value, x = stc,
  clab = bquote(c[X]),
  xlab = "Prior strength",
  ylab = "% agents in an echo chamber",
  limits = c(0, 101),
  pal = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5, -1)[c(1:3, 5)]
)
# ebox_p_cx
ebox_p_bt <- box_custom(
  ebox_d_bt, colour = bt, y = value, x = stc,
  clab = bquote(c[T]),
  xlab = "Prior strength",
  ylab = "% agents in an echo chamber",
  limits = c(0, 101)
)
# ebox_p_bt
ebox_p_all <- plot_grid(ebox_p_bt, ebox_p_cx, ebox_p, ncol = 1, labels = "AUTO")
ebox_p_all

# Opinion
opbox_p <- box_custom(
  opbox_d, x = cx, y = value, colour = bt,
  bquote(c[X]), bquote(c[T]), "Mean opinion",
  limits = c(0, 1)
)
# opbox_p
opbox_p_cx <- box_custom(
  opbox_d_cx, colour = cx, y = value, x = stc,
  clab = bquote(c[X]),
  xlab = "Prior strength",
  ylab = "Mean opinion",
  limits = c(0, 1),
  pal = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5, -1)[c(1:3, 5)]
)
# opbox_p_cx
opbox_p_bt <- box_custom(
  opbox_d_bt, colour = bt, y = value, x = stc,
  clab = bquote(c[T]),
  xlab = "Prior strength",
  ylab = "Mean opinion",
  limits = c(0, 1)
)
# opbox_p_bt
opbox_p_all <-
  plot_grid(opbox_p_bt, opbox_p_cx, opbox_p, ncol = 1, labels = "AUTO")
opbox_p_all

# Number of shares -------------------------------------------------------------
results_sh <- sapply(
  models2,
  function(x) {
    sapply(readRDS(file.path("results", paste0(x, "_r.rds")))$shares, colSums)
  },
  simplify = FALSE
)
shdata <- sapply(
  results_sh,
  function(x) {
    tmp <- data.frame(x)
    data.frame(tx = tmp$tx11 + tmp$tx10, a = tmp$a11 + tmp$a10)
  },
  simplify = FALSE
)
shdata$base$tx |> describe()
shdata$bt_0.5_cx_0.5$tx |> describe()

# Save plots -------------------------------------------------------------------
ggsave2(
  file.path("plots", "echo_box.png"), ebox_p_all,
  width = 4000, height = 3000, units = "px"
)
ggsave2(
  file.path("plots", "op_box.png"), opbox_p_all,
  width = 4000, height = 3000, units = "px"
)
ggsave2(
  file.path("plots", "Fig5.eps"), ebox_p_all,
  width = 2250, height = 2250, units = "px", dpi = 300
)
ggsave2(
  file.path("plots", "Fig6.eps"), opbox_p_all,
  width = 2250, height = 2250, units = "px", dpi = 300
)
