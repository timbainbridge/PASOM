############################ Progression plots #################################
#
# Produces Figure 1 for the paper and equivalent results for other simulations
# (not used in outputs anywhere).
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Create directory if needed
if (!dir.exists("plots")) dir.create("plots")

# Models to examine ------------------------------------------------------------

models <- c(
  "base_np"
  , "cx0_np"
  , "cx5_np"
  , "cx1_np"
  , "cx15_np"
  , "lattice_np"
  , "stcon_high_np"
  , "g1000_np"
  , "bt1_np"
  , "d0_np"
  , "cb1_np"
  , "g1_np"
  , "g2_np"
  , "sdswap_np"
  , "ck8_np"
  , "sd5_np"
)

# Create plots -----------------------------------------------------------------

for (i in models) {
  model <- paste0(i, "_r.rds")
  source(file.path("scripts", "results_s.R"))
  saveRDS(plots, file.path("plots", paste0(i, "_p.rds")))
  ggsave(file.path("plots", paste0(i, ".png")), plots,
         width = 1500, height = 5000, units = "px")
  if (i == "base_np") {
    ggsave(file.path("plots", "Figure1.eps"), plots,
           width = 6, height = 7.5, dpi = 450)
  }
}
