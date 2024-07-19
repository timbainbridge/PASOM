################## Generic Script for Interpreting the models ##################

# Load packages ----------------------------------------------------------------
pkgs <- list(
  "igraph"        # For networks
  , "psych"       # For describe()
)
sapply(pkgs, function(x) library(x, character.only = TRUE)) |> invisible()

# Load output ------------------------------------------------------------------
output <- readRDS(file.path("output", model))

# Final round ------------------------------------------------------------------
fround <- sapply(output, function(x) x$final_round)

# Interest ---------------------------------------------------------------------
lmda <- sapply(output, function(x) (x$lmda == 1) |> colSums())

# Shares by round --------------------------------------------------------------
shares <- list(a11 = sapply(output, function(x) x$a11 |> colSums()),
               a10 = sapply(output, function(x) x$a10 |> colSums()),
               tx11 = sapply(output, function(x) x$tx11 |> colSums()),
               tx10 = sapply(output, function(x) x$tx10 |> colSums()))

# Opinion tracking -------------------------------------------------------------
opinion_stats <- list(
  # Mean and SD for each round (SD ~ polarisation; M ~ leaning)
  mean = sapply(output, function(x) x$opinion1 |> colMeans()),
  sd = sapply(output, function(x) x$opinion1 |> apply(2, "sd")),
  final = mapply(function(x, y) x$opinion1[, y], x = output, y = fround)
)

# Echo chambers? ---------------------------------------------------------------
echo <- list(
  # echo = sapply(output, function(x) x$echo1 |> colMeans()),
  echo0 = sapply(output,
                 function(x) (x$echo == -1) |> colMeans()),
  echo1 = sapply(output,
                 function(x) (x$echo == 1) |> colMeans())
)

# Homophily --------------------------------------------------------------------
homoph <- mapply(
  function(x, y) {
    assortativity(x$g, y, directed = FALSE)
  },
  x = output, y = as.data.frame(opinion_stats$final)
)

# G-Size -----------------------------------------------------------------------
gsz <- list(start = sapply(output, function(x) x$g0 |> gsize()),
            end = sapply(output, function(x) x$g |> gsize()))

# Max degree -------------------------------------------------------------------
mdeg <- list(start = output[[1]]$g0 |> distances() |> max(),
             end = sapply(output, function(x) x$g |> distances() |> max()))

# Remove output object (may cause things to run quicker) -----------------------
rm(output)

# Return results ---------------------------------------------------------------
outputr <- list(fround = fround,
                shares = shares,
                opinion = opinion_stats,
                echo = echo,
                gsize = gsz,
                lambda = lmda,
                mxdegree = mdeg,
                homophily = homoph)