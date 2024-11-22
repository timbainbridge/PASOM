############################# Produce the outputs ##############################
#
# This file organises model outputs into a more useful form.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Results save location
if (!dir.exists("results")) dir.create("results")

# Models to examine ------------------------------------------------------------

# Comment out models you are not interested in or have already created the main
# results from.

models <- c(
  # "base_np"
  # , "cx0_np"
  # , "cx5_np"
  # , "cx1_np"
  # , "cx15_np"
  # , "lattice_np"
  # , "stcon_high_np"
  # , "g1000_np"
  # , "bt1_np"
  # , "d0_np"
  # , "cb1_np"
  # , "g1_np"
  # , "g2_np"
  # , "sdswap_np"
  # , "ck8_np"
  # , "sd5_np"
  "test"
)

# Extract outputs --------------------------------------------------------------

for (i in models) {
  model <- paste0(i, ".rds")
  source(file.path("scripts", "output_s.R"))
  saveRDS(outputr, file.path("results", paste0(i, "_r.rds")))
}
