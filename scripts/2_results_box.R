################ A file to produce boxplots for the paper ######################
#
# Produces Figures 3, 4, and 6 from the paper.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Load packages ----------------------------------------------------------------
pkgs <- list(
  "ggplot2"       # For all the figures
  , "ggpubr"      # For ggarrange()
  , "reshape"     # For melt()
  , "paletteer"   # For palettes
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Create output directory (if it doesn't exist) --------------------------------
if (!dir.exists("plots")) dir.create("plots")

# Data -------------------------------------------------------------------------
models2 <- c(
  "base_np",
  "cx15_np"
  , "cx1_np"
  , "cx5_np"
  , "cx0_np"
)
results2 <- sapply(
  models2,
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
    names(echo2) <- c("lower", "upper", "pseudomedian",
                      "lowert", "uppert", "mean", "median", "round")
    echo10 <- tmp$echo$echo1[200, ] * 100
    echo1w <- wilcox.test(echo10, conf.int = TRUE, mu = -.001)
    echo11 <- echo1w$conf.int
    echo1pm <- echo1w$estimate
    echo1t <- t.test(echo10)$conf.int
    echo1m <- echo10 |> mean()
    echo1md <- echo10 |> median()
    echo1 <- c(echo11, echo1pm, echo1t, echo1m, echo1md, 200)
    names(echo1) <- c("lower", "upper", "pseudomedian",
                      "lowert", "uppert", "mean", "median", "round")
    echo00 <- tmp$echo$echo0[200, ] * 100
    echo0w <- wilcox.test(echo00, conf.int = TRUE, mu = -.001)
    echo01 <- echo0w$conf.int
    echo0pm <- echo0w$estimate
    echo0t <- t.test(echo00)$conf.int
    echo0m <- echo00 |> mean()
    echo0md <- echo00 |> median()
    echo0 <- c(echo01, echo0pm, echo0t, echo0m, echo0md, 200)
    names(echo0) <- c("lower", "upper", "pseudomedian",
                      "lowert", "uppert", "mean", "median", "round")
    opm0 <- tmp$opinion$mean[200, ]
    opmw <- wilcox.test(opm0, conf.int = TRUE)
    opm1 <- opmw$conf.int
    opmpm <- opmw$estimate
    opmt <- t.test(opm0)$conf.int
    opmm <- opm0 |> mean()
    opmmd <- opm0 |> median()
    opm <- c(opm1, opmpm, opmt, opmm, opmmd, 200)
    names(opm) <- c("lower", "upper", "pseudomedian",
                    "lowert", "uppert", "mean", "median", "round")
    ppro <- (tmp$opinion$final > .5) |> colMeans()
    panti <- (tmp$opinion$final < .5) |> colMeans()
    homophily <- tmp$homophily
    sdop <- tmp$opinion$sd[200, ]
    list(echo1 = echo1, echo10 = echo10, echo0 = echo0, echo00 = echo00,
         echo2 = echo2, echo20 = echo20, opm = opm, opm0 = opm0,
         homophily = homophily, sd = sdop, ppro = ppro, panti = panti)
  },
  simplify = FALSE
)

# Specific results for the paper -----------------------------------------------
# Number of simulations with < 70% in an echo chamber.
sum(results2$base_np$echo20 < 70)
(sum(results2$base_np$ppro == 1) + sum(results2$base_np$panti == 1))/200
sum(results2$base_np$ppro == 1) /
  (sum(results2$base_np$ppro == 1) + sum(results2$base_np$panti == 1))

# Opinion ----------------------------------------------------------------------
mdata2 <- cbind(
  do.call(rbind,
          sapply(results2, function(x) x$opm, simplify = FALSE)),
  cx = 4:0*.5
) |> data.frame()
mdata2$cx <- mdata2$cx |> sprintf(fmt = "%0.1f")
mdata20 <- cbind(
  melt(sapply(results2, function(x) x$opm0) |> data.frame()),
  iter = rep(1:200, 5),
  cx = rep(4:0*.5, each = 200)
) |> data.frame()
names(mdata20)[names(mdata20) == "value"] <- "mean"
mdata20$cx <- mdata20$cx |> sprintf(fmt = "%0.1f")

# Anti-science echo chambers ---------------------------------------------------
e0data2 <- cbind(
  do.call(rbind,
          sapply(results2, function(x) x$echo0, simplify = FALSE)),
  cx = 4:0*.5
) |> data.frame()
e0data2$cx <- e0data2$cx |> sprintf(fmt = "%0.1f")
e0data20 <- cbind(
  melt(sapply(results2, function(x) x$echo00) |> data.frame()),
  iter = rep(1:200, 5),
  cx = rep(4:0*.5, each = 200)
) |> data.frame()
names(e0data20)[names(e0data20) == "value"] <- "mean"
e0data20$cx = e0data20$cx |> sprintf(fmt = "%0.1f")

# Pro-science echo chambers ---------------------------------------------------
e1data2 <- cbind(
  do.call(rbind,
          sapply(results2, function(x) x$echo1, simplify = FALSE)),
  cx = 4:0*.5
) |> data.frame()
e1data2$cx <- e1data2$cx |> sprintf(fmt = "%0.1f")
e1data20 <- cbind(
  melt(sapply(results2, function(x) x$echo10) |> data.frame()),
  iter = rep(1:200, 5),
  cx = rep(4:0*.5, each = 200)
) |> data.frame()
names(e1data20)[names(e1data20) == "value"] <- "mean"
e1data20$cx = e1data20$cx |> sprintf(fmt = "%0.1f")

# Any echo chamber -------------------------------------------------------------
edata2 <- cbind(
  do.call(rbind,
          sapply(results2, function(x) x$echo2, simplify = FALSE)),
  cx = 4:0*.5
) |> data.frame()
edata2$cx <- edata2$cx |> sprintf(fmt = "%0.1f")
edata20 <- cbind(
  melt(sapply(results2, function(x) x$echo20) |> data.frame()),
  iter = rep(1:200, 5),
  cx = rep(4:0*.5, each = 200)
) |> data.frame()
names(edata20)[names(edata20) == "value"] <- "mean"
edata20$cx = edata20$cx |> sprintf(fmt = "%0.1f")

# Box plots --------------------------------------------------------------------

# Opinion ----------------------------------------------------------------------
mboxplot <-
  ggplot(mdata20, aes(x = cx, y = mean)) +
  geom_boxplot(alpha = 0) +
  geom_point(aes(x = cx, y = mean, colour = cx)
             , position = position_jitterdodge(jitter.width = .1)) +
  scale_colour_manual(
    values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5)
  ) +
  labs(x = bquote(c[x]), colour = bquote(c[x])) +
  scale_y_continuous("Mean opinion",
                     0:5*.2,
                     0:4*.2+.1) +
  coord_cartesian(c(.5, 5.5), c(0, 1), FALSE) +
  theme_bw() +
  theme(text = element_text(size = 10),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"),
        legend.position = "none") +
  ggtitle("(A)")
mboxplot

# Anti-science echo chambers ---------------------------------------------------
e0boxplot <-
  ggplot(e0data20, aes(x = cx, y = mean)) +
  geom_boxplot(
    # notch = TRUE,
    alpha = 0
  ) +
  geom_point(aes(x = cx, y = mean, colour = cx)
             , position = position_jitterdodge(jitter.width = .1)) +
  scale_colour_manual(
    values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5)
  ) +
  labs(x = bquote(c[x]), colour = bquote(c[x])) +
  scale_y_continuous("% agents in an anti-science Echo Chamber",
                     0:5*20,
                     0:4*20+10) +
  coord_cartesian(c(.5, 5.5), c(0, 100), FALSE) +
  theme_bw() +
  theme(text = element_text(size = 10),
        plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
  ggtitle("(A)")
e0boxplot

# Pro-science echo chambers ---------------------------------------------------
e1boxplot <-
  ggplot(e1data20, aes(x = cx, y = mean)) +
  geom_boxplot(alpha = 0) +
  geom_point(aes(x = cx, y = mean, colour = cx)
             , position = position_jitterdodge(jitter.width = .1)) +
  scale_colour_manual(
    values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5)
  ) +
  labs(x = bquote(c[x]), colour = bquote(c[x])) +
  scale_y_continuous("% agents in a pro-science Echo Chamber",
                     0:5*20,
                     0:4*20+10) +
  coord_cartesian(c(.5, 5.5), c(0, 100), FALSE) +
  theme_bw() +
  theme(text = element_text(size = 10),
        plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
  ggtitle("(B)")
e1boxplot

# Any echo chamber -------------------------------------------------------------
eboxplot <-
  ggplot(edata20, aes(x = cx, y = mean)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_point(aes(x = cx, y = mean, colour = cx),
             , position = position_jitterdodge(jitter.width = .1)) +
  scale_colour_manual(
    values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5)
  ) +
  labs(x = bquote(c[x]), colour = bquote(c[x])) +
  scale_y_continuous("% agents in any Echo Chamber",
                     0:5*20,
                     0:4*20+10) +
  coord_cartesian(c(.5, 5.5), c(0, 100), FALSE) +
  theme_bw() +
  theme(text = element_text(size = 10),
        plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
  ggtitle("(C)")
eboxplot
 
# # Combined EC plots ----------------------------------------------------------
eboxall <- ggarrange(plotlist = list(e0boxplot, e1boxplot, eboxplot),
                     ncol = 3,
                     common.legend = TRUE,
                     legend = "right")
eboxall

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

# Tx (toxic) -------------------------------------------------------------------
txdata <- cbind(
  melt(sapply(shdata, function(x) x$tx) |> data.frame()),
  iter = rep(1:200, 5),
  cx = rep(4:0*.5, each = 200)
) |> data.frame()
names(txdata)[names(txdata) == "value"] <- "shares"
txdata$cx <- txdata$cx |> sprintf(fmt = "%0.1f")
txplot <-
  ggplot(txdata, aes(x = cx, y = shares)) +
  geom_boxplot(notch = TRUE, alpha = 0) +
  geom_point(aes(x = cx, y = shares, colour = cx)
             , position = position_jitterdodge(jitter.width = .1)) +
  scale_colour_manual(
    values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5)
  ) +
  labs(x = bquote(c[x]), y = "Toxic Shares", colour = bquote(c[x])) +
  theme_bw() +
  theme(text = element_text(size = 10),
        plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
  ggtitle("(A)")
txplot

# A (constructive) -------------------------------------------------------------
adata <- cbind(
  melt(sapply(shdata, function(x) x$a) |> data.frame()),
  iter = rep(1:200, 5),
  cx = rep(4:0*.5, each = 200)
) |> data.frame()
names(adata)[names(adata) == "value"] <- "shares"
adata$cx <- adata$cx |> sprintf(fmt = "%0.1f")
aplot <-
  ggplot(adata, aes(x = cx, y = shares)) +
  geom_boxplot(notch = TRUE, alpha = 0) +
  geom_point(aes(x = cx, y = shares, colour = cx)
             , position = position_jitterdodge(jitter.width = .1)) +
  scale_colour_manual(
    values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5)
  ) +
  labs(x = bquote(c[x]), y = "Constructive Shares", colour = bquote(c[x])) +
  coord_cartesian(c(.5, 5.5), c(0, 54000), FALSE) +
  theme_bw() +
  theme(text = element_text(size = 10),
        plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
  ggtitle("(B)")
aplot

# Combined Tx and A plots ------------------------------------------------------
txaplot <- ggarrange(plotlist = list(txplot, aplot),
                     ncol = 2,
                     common.legend = TRUE,
                     legend = "right")
txaplot

# SD plot ----------------------------------------------------------------------
sddata0 <- do.call(
  rbind,
  mapply(
    function(x, y) {
      data.frame(cx = rep(y, length(x$sd)), sd = x$sd)
    },
    x = results2, y = 4:0*.5, SIMPLIFY = FALSE
  )
)
sddata0$cx <- sddata0$cx |> sprintf(fmt = "%0.1f")

sdboxplot <-
  ggplot(sddata0, aes(x = cx, y = sd)) +
  geom_boxplot(alpha = 0) +
  geom_point(aes(x = cx, y = sd, colour = cx)
             , position = position_jitterdodge(jitter.width = .1)) +
  scale_colour_manual(values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 5)) +
  labs(x = bquote(c[x]), colour = bquote(c[x])) +
  scale_y_continuous("SD of opinions") +
  theme_bw() +
  theme(text = element_text(size = 10),
        plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
  ggtitle("(B)")
sdboxplot

msdarrange <- ggarrange(plotlist = list(mboxplot, sdboxplot),
                        ncol = 2,
                        common.legend = TRUE,
                        legend = "right")
msdarrange

# Save plots -------------------------------------------------------------------
ggsave(file.path("plots", "e_box.png"), eboxall,
       width = 3000, height = 1500, units = "px")
ggsave(file.path("plots", "Figure3.eps"), eboxall,
       width = 6, height = 4.5, dpi = 450)
ggsave(file.path("plots", "txa_plot.png"), txaplot,
       width = 2200, height = 1500, units = "px")
ggsave(file.path("plots", "Figure6.eps"), txaplot,
       width = 6, height = 5, dpi = 450)
ggsave(file.path("plots", "msd_plot.png"), msdarrange,
       width = 2200, height = 1500, units = "px")
ggsave(file.path("plots", "Figure4.eps"), msdarrange,
       width = 6, height = 5, dpi = 450)
