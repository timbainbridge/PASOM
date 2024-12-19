################ A file to produce the results for the models ##################
#
# This file is a called by `2_results_time.R` to produce time series plots of
# results from the simulations for each of the varieties listed in the upstream
# file.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Load packages ----------------------------------------------------------------
pkgs <- list(
  "ggplot2"
  , "ggpubr"
  , "ggnewscale"
  , "reshape"
  , "paletteer"
  , "cowplot"
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Load results -----------------------------------------------------------------
if (!exists("results")) {
  results <- readRDS(file.path("results", paste0(model, "_r.rds")))
}

# Values at final round --------------------------------------------------
echo1 <- results$echo$echo1 * 100  # as percentage
echo0 <- results$echo$echo0 * 100  # as percentage
echo <- echo0[nrow(echo0), ]
echo2 <- echo1[nrow(echo1), ] + echo0[nrow(echo0), ]
opm0 <- results$opinion$mean
opm <- opm0[nrow(opm0), ]
# lmda <- results$lambda

# M over time ------------------------------------------------------------------

# Data
mdata0 <- as.data.frame(opm0)
mdata0$round <- row.names(mdata0) |> as.numeric()
mdata <- melt(mdata0, id.vars = "round", variable_name = "Iteration")
mdata$echo <- echo[match(mdata$Iteration, names(echo))]

# Plot
mplot <-
  ggplot() +
  geom_line(
    aes(x = round, y = value, group = Iteration, colour = echo),
    mdata
  ) +
  scale_colour_gradientn(
    colours = paletteer_c("ggthemes::Red-Gold", 101), limits = 0:1 * 100
  ) +
  scale_y_continuous(
    "Mean of\nOpinion",
    1:5*.2-.1,
    0:5*.2,
    limits = c(0, 1),
    expand = c(.02, .02)
  ) +
  labs(x = "Round", colour = "% agents in\nanti-science\nEC at\nfinal round") +
  theme_bw()
  
mplot2 <-
  mplot +
  new_scale_colour() +
  geom_line(
    aes(x = round, y = value),
    cbind(
      value = rowMeans(mdata0[, !grepl("round", names(mdata0))]),
      mdata0["round"]
    ),
    colour = "black",
    linewidth = 2
  ) #+
  # ggtitle("(D)")

# Echo chambers ----------------------------------------------------------------
# Anti-science
e0data0 <- as.data.frame(echo0)
e0data0$round <- row.names(e0data0) |> as.numeric()
e0data <- melt(e0data0, id.vars = "round", variable_name = "Iteration")
e0data$opinion <- opm[match(e0data$Iteration, names(opm))]

e0plot <-
  ggplot() +
  geom_line(
    aes(x = round, y = value, group = Iteration, colour = opinion),
    e0data
  ) +
  scale_colour_gradientn(
    colours = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 101, -1),
    limits = 0:1
  ) +
  scale_y_continuous(
    "% agents in anti-science\nEcho Chamber",
    0:5*20,
    1:5*20 - 10,
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  labs(x = "Round", colour = "Mean opinion\nat final round") +
  theme_bw()
  
e0plot2 <-
  e0plot +
  new_scale_colour() +
  geom_line(
    aes(x = round, y = value),
    cbind(value = rowMeans(e0data0[, -ncol(e0data0)]), e0data0["round"]),
    colour = "black",
    linewidth = 2
  ) #+
  # ggtitle("(A)")

# Pro-science
e1data0 <- as.data.frame(echo1)
e1data0$round <- row.names(e1data0) |> as.numeric()
e1data <- melt(e1data0, id.vars = "round", variable_name = "Iteration")
e1data$opinion <- opm[match(e1data$Iteration, names(opm))]

e1plot <-
  ggplot() +
  geom_line(
    aes(x = round, y = value, group = Iteration, colour = opinion),
    e1data
  ) +
  scale_colour_gradientn(
    colours = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 101, -1),
    limits = 0:1
  ) +
  scale_y_continuous(
    "% agents in pro-science\nEcho Chamber",
    0:5*20,
    1:5*20 - 10,
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  labs(x = "Round", colour = "Mean opinion\nat final round") +
  theme_bw()

e1plot2 <-
  e1plot +
  new_scale_colour() +
  geom_line(
    aes(x = round, y = value),
    cbind(value = rowMeans(e1data0[, -ncol(e1data0)]), e1data0["round"]),
    colour = "black",
    linewidth = 2
  ) #+
  # ggtitle("(B)")

# Combined
edata0 <- as.data.frame(echo0 + echo1)
edata0$round <- row.names(edata0) |> as.numeric()
edata <- melt(edata0, id.vars = "round", variable_name = "Iteration")
edata$opinion <- opm[match(edata$Iteration, names(opm))]

eplot <-
  ggplot() +
  geom_line(
    aes(x = round, y = value, group = Iteration, colour = opinion),
    edata
  ) +
  scale_colour_gradientn(
    colours = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 101, -1),
    limits = 0:1
  ) +
  scale_y_continuous(
    "% agents in any\nEcho Chamber",
    0:5*20,
    1:5*20 - 10,
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  labs(x = "Round", colour = "Mean opinion\nat final round") +
  theme_bw()

eplot2 <-
  eplot +
  new_scale_colour() +
  geom_line(
    aes(x = round, y = value),
    cbind(value = rowMeans(edata0[, -ncol(edata0)]), edata0["round"]),
    colour = "black",
    linewidth = 2
  ) #+
  # ggtitle("(C)")

# Combined Mean and Echo plots -------------------------------------------------
meplot <- plot_grid(
  plotlist = list(e0plot2, e1plot2, eplot2, mplot2),
  nrow = 2, ncol = 2,
  # nrow = 4,
  labels = "AUTO"
)

# Return results ---------------------------------------------------------------
plots <- meplot
