################ A file to produce boxplots for the paper ######################
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

# # Figure
# g_ep <-
#   ggplot(e_box_d[e_box_d$stc == 40, ], aes(fill = bt, y = mean, x = cx)) +
#   geom_bar(position = "dodge", stat = "identity", colour = "black") +
#   geom_errorbar(
#     aes(ymin = lowert, ymax = uppert), position = position_dodge(.9)
#   ) +
#   theme_bw() +
#   labs(x = bquote(c[X]), y = "% agents in an echo chamber", fill = bquote(c[T])) +
#   scale_fill_manual(
#     values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 7, -1)
#   )
# # g_ep
# g_ep2 <-
#   ggplot(e_box_d[e_box_d$stc == 40, ], aes(fill = cx, y = mean, x = bt)) +
#   geom_bar(position = "dodge", stat = "identity", colour = "black") +
#   geom_errorbar(
#     aes(ymin = lowert, ymax = uppert), position = position_dodge(.9)
#   ) +
#   theme_bw() +
#   xlab(bquote(b[T])) +
#   ylab("% agents in an echo chamber") +
#   scale_fill_manual(
#     values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 4, -1)
#   )
# g_ep2


# g_ep_bt <-
#   ggplot(e_box_d_bt, aes(fill = stc, y = mean, x = bt)) +
#   geom_bar(
#     position = position_dodge2(reverse = TRUE),
#     stat = "identity",
#     colour = "black"
#   ) +
#   geom_errorbar(
#     aes(ymin = lowert, ymax = uppert),
#     position = position_dodge2(.9, reverse = TRUE)
#   ) +
#   theme_bw() +
#   labs(
#     x = bquote(c[T]),
#     y = "% agents in an echo chamber",
#     fill = "Prior\nstrength"
#   ) +
#   scale_fill_manual(
#     values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 3, -1)
#   )
# g_ep_bt

# g_ep_bt2 <-
#   ggplot(e_box_d_bt, aes(fill = bt, y = mean, x = stc)) +
#   geom_bar(
#     position = position_dodge2(),
#     stat = "identity",
#     colour = "black"
#   ) +
#   geom_errorbar(
#     aes(ymin = lowert, ymax = uppert),
#     position = position_dodge2(.9)
#   ) +
#   theme_bw() +
#   labs(
#     fill = bquote(c[T]),
#     y = "% agents in an echo chamber",
#     x = "Prior strength"
#   ) +
#   scale_fill_manual(
#     values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 7, -1) #|>  # Alt = 36
#       # _[c(1, 6, 9, 11, 13, 24, 36)]
#   ) +
#   scale_y_continuous(limits = c(0, 100) , expand = c(0, 0)) +
#   scale_x_discrete(label = c("Weak", "Moderate", "Strong"))
# g_ep_bt2

# Select cx

# g_ep_cx <-
#   ggplot(e_box_d_cx, aes(fill = stc, y = mean, x = cx)) +
#   geom_bar(
#     position = position_dodge2(reverse = TRUE),
#     stat = "identity",
#     colour = "black"
#   ) +
#   geom_errorbar(
#     aes(ymin = lowert, ymax = uppert),
#     position = position_dodge2(.9, reverse = TRUE)
#   ) +
#   theme_bw() +
#   labs(
#     x = bquote(c[X]),
#     y = "% agents in an echo chamber",
#     fill = "Prior\nstrength"
#   ) +
#   scale_fill_manual(
#     values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 3, -1)
#   ) +
#   scale_y_continuous(limits = c(0, 100) , expand = c(0, 0))
# g_ep_cx

# g_ep_cx2 <-
#   ggplot(e_box_d_cx, aes(x = stc, y = mean, fill = cx)) +
#   geom_bar(
#     position = position_dodge2(),
#     stat = "identity",
#     colour = "black"
#   ) +
#   geom_errorbar(
#     aes(ymin = lowert, ymax = uppert),
#     position = position_dodge2(.9)
#   ) +
#   theme_bw() +
#   labs(
#     fill = bquote(c[X]), y = "% agents in an echo chamber", x = "Prior strength"
#   ) +
#   scale_fill_manual(
#     values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5, -1)[c(1:3, 5)]
#   ) +
#   scale_y_continuous(limits = c(0, 100) , expand = c(0, 0)) +
#   scale_x_discrete(label = c("Weak", "Moderate", "Strong"))
# g_ep_cx2

# g_ep_comb <- plot_grid(
#   g_ep_bt2, g_ep_cx2, g_ep,
#   ncol = 1,
#   labels = "AUTO"
# )
# g_ep_comb

# Figure -----------------------------------------------------------------------

# Echo chambers
ebox_p <- box_custom(
  ebox_d, x = cx, y = value, colour = bt,
  bquote(c[X]), bquote(c[T]), "% agents in an echo chamber",
  limits = c(0, 101)
)
ebox_p
ebox_p_cx <- box_custom(
  ebox_d_cx, colour = cx, y = value, x = stc,
  clab = bquote(c[X]),
  xlab = "Prior strength",
  ylab = "% agents in an echo chamber",
  limits = c(0, 101),
  pal = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5, -1)[c(1:3, 5)]
)
ebox_p_cx
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
shdata$cx_0.1_stc_40$tx |> describe()
shdata$bt_0.5_cx_0.5$tx |> describe()

# # Tx - Mean by model -----------------------------------------------------------
# describe(shdata$base$tx)
# describe(shdata$bt_0_cx_0.5$tx)

# # Tx (toxic) -------------------------------------------------------------------
# txdata <- cbind(
#   melt(sapply(shdata, function(x) x$tx) |> data.frame()),
#   iter = rep(1:200, 5),
#   cx = rep(4:0*.5, each = 200)
# ) |> data.frame()
# names(txdata)[names(txdata) == "value"] <- "shares"
# txdata$cx <- txdata$cx |> sprintf(fmt = "%0.1f")
# txplot <-
#   ggplot(txdata, aes(x = cx, y = shares)) +
#   geom_boxplot(notch = TRUE, alpha = 0) +
#   geom_point(aes(x = cx, y = shares, colour = cx)
#              , position = position_jitterdodge(jitter.width = .1)) +
#   scale_colour_manual(
#     values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5)
#   ) +
#   labs(x = bquote(c[x]), y = "Toxic Shares", colour = bquote(c[x])) +
#   theme_bw() +
#   theme(text = element_text(size = 10),
#         plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
#   ggtitle("(A)")
# txplot

# # A (constructive) -------------------------------------------------------------
# adata <- cbind(
#   melt(sapply(shdata, function(x) x$a) |> data.frame()),
#   iter = rep(1:200, 5),
#   cx = rep(4:0*.5, each = 200)
# ) |>
#   data.frame()
# names(adata)[names(adata) == "value"] <- "shares"
# adata$cx <- adata$cx |> sprintf(fmt = "%0.1f")
# aplot <-
#   ggplot(adata, aes(x = cx, y = shares)) +
#   geom_boxplot(notch = TRUE, alpha = 0) +
#   geom_point(
#     aes(x = cx, y = shares, colour = cx),
#     position = position_jitterdodge(jitter.width = .1)
#   ) +
#   scale_colour_manual(
#     values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5)
#   ) +
#   labs(x = bquote(c[x]), y = "Constructive Shares", colour = bquote(c[x])) +
#   coord_cartesian(c(.5, 5.5), c(0, 54000), FALSE) +
#   theme_bw() +
#   theme(
#     text = element_text(size = 10),
#     plot.margin = unit(c(.5, .5, .5, .5), "cm")
#   ) +
#   ggtitle("(B)")
# aplot

# # Combined Tx and A plots ------------------------------------------------------
# txaplot <- ggarrange(
#   plotlist = list(txplot, aplot),
#   ncol = 2,
#   common.legend = TRUE,
#   legend = "right"
# )
# txaplot

# # SD plot ----------------------------------------------------------------------
# sddata0 <- do.call(
#   rbind,
#   mapply(
#     function(x, y) {
#       data.frame(cx = rep(y, length(x$sd)), sd = x$sd)
#     },
#     x = results2, y = 4:0*.5, SIMPLIFY = FALSE
#   )
# )
# sddata0$cx <- sddata0$cx |> sprintf(fmt = "%0.1f")
# 
# sdboxplot <-
#   ggplot(sddata0, aes(x = cx, y = sd)) +
#   geom_boxplot(alpha = 0) +
#   geom_point(aes(x = cx, y = sd, colour = cx)
#              , position = position_jitterdodge(jitter.width = .1)) +
#   scale_colour_manual(
#     values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5)
#   ) +
#   labs(x = bquote(c[x]), colour = bquote(c[x])) +
#   scale_y_continuous("SD of opinions") +
#   theme_bw() +
#   theme(
#     text = element_text(size = 10),
#     plot.margin = unit(c(.5, .5, .5, .5), "cm")
#   ) +
#   ggtitle("(B)")
# sdboxplot
# 
# msdarrange <- ggarrange(
#   plotlist = list(mboxplot, sdboxplot),
#   ncol = 2,
#   common.legend = TRUE,
#   legend = "right"
# )
# msdarrange

# Save plots -------------------------------------------------------------------
# ggsave(file.path("plots", "e_box.png"), eboxall,
#        width = 3000, height = 1500, units = "px")
# ggsave(file.path("plots", "Figure3.eps"), eboxall,
#        width = 6, height = 4.5, dpi = 450)
# ggsave(file.path("plots", "txa_plot.png"), txaplot,
#        width = 2200, height = 1500, units = "px")
# ggsave(file.path("plots", "Figure6.eps"), txaplot,
#        width = 6, height = 5, dpi = 450)
# ggsave(file.path("plots", "msd_plot.png"), msdarrange,
#        width = 2200, height = 1500, units = "px")
# ggsave(file.path("plots", "Figure4.eps"), msdarrange,
#        width = 6, height = 5, dpi = 450)
# ggsave2(file.path("plots", "EchoByParam.png"), g_ep, width = 12, height = 8)
# ggsave2(
#   file.path("plots", "echo_bar.png"), g_ep_comb,
#   width = 4000, height = 3000, units = "px"
# )
ggsave2(
  file.path("plots", "echo_box.png"), ebox_p_all,
  width = 4000, height = 3000, units = "px"
)
ggsave2(
  file.path("plots", "op_box.png"), opbox_p_all,
  width = 4000, height = 3000, units = "px"
)
